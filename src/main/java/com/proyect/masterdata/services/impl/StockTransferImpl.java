package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.StockTransferDTO;
import com.proyect.masterdata.dto.request.RequestStockTransactionItem;
import com.proyect.masterdata.dto.request.RequestStockTransfer;
import com.proyect.masterdata.dto.request.RequestStockTransferItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IStockTransaction;
import com.proyect.masterdata.services.IStockTransfer;
import com.proyect.masterdata.services.IStockTransferItem;
import com.proyect.masterdata.services.IWarehouseStock;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;

@Service
@RequiredArgsConstructor
@Log4j2
public class StockTransferImpl implements IStockTransfer {
    private final UserRepository userRepository;
    private final WarehouseRepository warehouseRepository;
    private final WarehouseStockRepository warehouseStockRepository;
    private final SupplierProductRepository supplierProductRepository;
    private final StockTransferRepository stockTransferRepository;
    private final IStockTransferItem iStockTransferItem;
    private final IStockTransaction iStockTransaction;
    private final IWarehouseStock iWarehouseStock;
    private final StockTransferRepositoryCustom stockTransferRepositoryCustom;
    @Override
    public ResponseSuccess save(RequestStockTransfer requestStockTransfer,String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {

        User user;
        Warehouse originWarehouse;
        Warehouse destinationWarehouse;

        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            originWarehouse = warehouseRepository.findByNameAndStatusTrue(requestStockTransfer.getOriginWarehouse().toUpperCase());
            destinationWarehouse = warehouseRepository.findByNameAndStatusTrue(requestStockTransfer.getDestinationWarehouse().toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(originWarehouse == null){
            throw new BadRequestExceptions(Constants.ErrorOriginWarehouse);
        }

        if(destinationWarehouse == null){
            throw new BadRequestExceptions(Constants.ErrorDestinationWarehouse);
        }

        try{
            for(RequestStockTransferItem requestStockTransferItem : requestStockTransfer.getRequestStockTransferItemList()){
                SupplierProduct supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(requestStockTransferItem.getSupplierProductSerial());
                WarehouseStock originWarehouseStock = warehouseStockRepository.findByWarehouseIdAndSupplierProductId(originWarehouse.getId(), supplierProduct.getId());

                if(originWarehouseStock.getQuantity() < requestStockTransferItem.getQuantity()){
                    throw new BadRequestExceptions(Constants.ErrorOriginWarehouseStock);
                }
            }

            StockTransfer newStockTransfer = stockTransferRepository.save(StockTransfer.builder()
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .updateDate(new Date(System.currentTimeMillis()))
                            .originWarehouse(originWarehouse)
                            .originWarehouseId(originWarehouse.getId())
                            .destinationWarehouse(destinationWarehouse)
                            .destinationWarehouseId(destinationWarehouse.getId())
                            .client(user.getClient())
                            .clientId(user.getClientId())
                            .tokenUser(user.getUsername())
                    .build());

            List<RequestStockTransactionItem> requestStockTransactionItemList = new ArrayList<>();

            for(RequestStockTransferItem requestStockTransferItem : requestStockTransfer.getRequestStockTransferItemList()){
                SupplierProduct supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(requestStockTransferItem.getSupplierProductSerial());
                requestStockTransactionItemList.add(RequestStockTransactionItem.builder()
                                .quantity(requestStockTransferItem.getQuantity())
                                .supplierProductSerial(requestStockTransferItem.getSupplierProductSerial())
                        .build());
                iStockTransferItem.save(requestStockTransferItem,newStockTransfer,supplierProduct,user);
                iWarehouseStock.out(originWarehouse,supplierProduct,requestStockTransferItem.getQuantity(),user);
                iWarehouseStock.in(destinationWarehouse,supplierProduct, requestStockTransferItem.getQuantity(), user);
            }

            iStockTransaction.save("STI"+newStockTransfer.getId(),originWarehouse,requestStockTransactionItemList,"TRANSFERENCIA-SALIDA",user);
            iStockTransaction.save("STO"+newStockTransfer.getId(), destinationWarehouse, requestStockTransactionItemList,"TRANSFERENCIA-ENTRADA",user);
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            e.printStackTrace();
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public Page<StockTransferDTO> list(String user, String originWarehouse, String destinationWarehouse, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions {

        Page<StockTransfer> pageStockTransfer;
        Long clientId;
        Long originWarehouseId;
        Long destinationWarehouseId;

        if(originWarehouse != null){
            originWarehouseId = warehouseRepository.findByNameAndStatusTrue(originWarehouse.toUpperCase()).getId();
        }else {
            originWarehouseId = null;
        }

        if(destinationWarehouse != null){
            destinationWarehouseId = warehouseRepository.findByNameAndStatusTrue(destinationWarehouse.toUpperCase()).getId();
        }else{
            destinationWarehouseId = null;
        }

        try{
            clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
            pageStockTransfer = stockTransferRepositoryCustom.searchForStockTransfer(clientId,originWarehouseId,destinationWarehouseId,sort,sortColumn,pageNumber,pageSize);
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.ResultsFound);
        }

        if(pageStockTransfer.isEmpty()){
            return new PageImpl<>(Collections.emptyList());
        }

        List<StockTransferDTO> stockTransferDTOS = pageStockTransfer.getContent().stream().map(stockTransfer -> StockTransferDTO.builder()
                .originWarehouse(stockTransfer.getOriginWarehouse().getName())
                .destinationWarehouse(stockTransfer.getDestinationWarehouse().getName())
                .registrationDate(stockTransfer.getRegistrationDate())
                .stockTransferId(stockTransfer.getId())
                .build()).toList();

        return new PageImpl<>(stockTransferDTOS,pageStockTransfer.getPageable(),pageStockTransfer.getTotalElements());
    }

    @Override
    public List<StockTransferDTO> listStockTransfer(String user) throws InternalErrorExceptions, BadRequestExceptions {
        List<StockTransfer> stockTransfers;
        Long clientId;
        try {
            clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
            stockTransfers = stockTransferRepository.findAllByClientId(clientId);
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if (stockTransfers.isEmpty()){
            return Collections.emptyList();
        }
        return stockTransfers.stream().map(stockTransfer -> StockTransferDTO.builder()
                .originWarehouse(stockTransfer.getOriginWarehouse().getName())
                .destinationWarehouse(stockTransfer.getDestinationWarehouse().getName())
                .registrationDate(stockTransfer.getRegistrationDate())
                .stockTransferId(stockTransfer.getId())
                .build()).toList();
    }
}
