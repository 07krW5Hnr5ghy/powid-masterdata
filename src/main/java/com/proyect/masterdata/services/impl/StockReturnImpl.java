package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.StockReturnDTO;
import com.proyect.masterdata.dto.request.RequestStockReturnItem;
import com.proyect.masterdata.dto.request.RequestStockTransactionItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IStockReturn;
import com.proyect.masterdata.services.IStockReturnItem;
import com.proyect.masterdata.services.IStockTransaction;
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
public class StockReturnImpl implements IStockReturn {
    private final PurchaseRepository purchaseRepository;
    private final UserRepository userRepository;
    private final StockReturnRepository stockReturnRepository;
    private final SupplierProductRepository supplierProductRepository;
    private final PurchaseItemRepository purchaseItemRepository;
    private final StockReturnRepositoryCustom stockReturnRepositoryCustom;
    private final IStockReturnItem iStockReturnItem;
    private final IStockTransaction iStockTransaction;
    private final ShipmentRepository shipmentRepository;
    private final IWarehouseStock iWarehouseStock;
    @Override
    public ResponseSuccess save(String purchaseSerial, List<RequestStockReturnItem> requestStockReturnItemList, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        Purchase purchase;
        StockReturn stockReturn;
        Shipment shipment;

        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            purchase = purchaseRepository.findBySerial(purchaseSerial);
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(purchase == null){
            throw new BadRequestExceptions(Constants.ErrorPurchase);
        }else{
            stockReturn = stockReturnRepository.findByPurchaseId(purchase.getId());
            shipment = shipmentRepository.findByPurchaseIdAndShipmentTypeName(purchase.getId(), "EMBARQUE");
        }

        if(stockReturn != null){
            throw new BadRequestExceptions(Constants.ErrorStockReturnExists);
        }

        if(shipment == null){
            throw new BadRequestExceptions(Constants.ErrorShipment);
        }

        try {
            for(RequestStockReturnItem requestStockReturnItem : requestStockReturnItemList){
                SupplierProduct supplierProduct = supplierProductRepository.findBySerial(requestStockReturnItem.getSupplierProductSerial());
                PurchaseItem purchaseItem = purchaseItemRepository.findByPurchaseIdAndSupplierProductId(purchase.getId(),supplierProduct.getId());
                if(requestStockReturnItem.getQuantity() > purchaseItem.getQuantity()){
                    throw new BadRequestExceptions(Constants.ErrorStockReturnQuantity);
                }
            }
            StockReturn newStockReturn = stockReturnRepository.save(StockReturn.builder()
                            .serial(purchase.getSerial())
                            .purchase(purchase)
                            .purchaseId(purchase.getId())
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .updateDate(new Date(System.currentTimeMillis()))
                            .client(user.getClient())
                            .clientId(user.getClientId())
                            .tokenUser(user.getUsername())
                            .status(true)
                    .build());
            List<RequestStockTransactionItem> requestStockTransactionItemList = new ArrayList<>();

            for(RequestStockReturnItem requestStockReturnItem : requestStockReturnItemList){
                SupplierProduct supplierProduct = supplierProductRepository.findBySerial(requestStockReturnItem.getSupplierProductSerial());
                PurchaseItem purchaseItem = purchaseItemRepository.findByPurchaseIdAndSupplierProductId(purchase.getId(),supplierProduct.getId());
                iStockReturnItem.save(newStockReturn,purchaseItem,requestStockReturnItem,user);
                iWarehouseStock.out(shipment.getWarehouse(),supplierProduct,requestStockReturnItem.getQuantity(),user);
                requestStockTransactionItemList.add(RequestStockTransactionItem.builder()
                                .supplierProductSerial(supplierProduct.getSerial())
                                .quantity(requestStockReturnItem.getQuantity())
                        .build());
            }

            iStockTransaction.save("DS"+newStockReturn.getId(),shipment.getWarehouse(),requestStockTransactionItemList,"DEVOLUCION-PROVEEDOR",user);

            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public Page<StockReturnDTO> list(String purchaseSerial, String user, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        Page<StockReturn> pageStockReturn;
        Long clientId;
        Long purchaseId;

        if(purchaseSerial != null){
            purchaseId = purchaseRepository.findBySerial(purchaseSerial.toUpperCase()).getId();
        }else {
            purchaseId = null;
        }

        try {
            clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
            pageStockReturn = stockReturnRepositoryCustom.searchForStockReturnItem(purchaseId,clientId,sort,sortColumn,pageNumber,pageSize,true);
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.ResultsFound);
        }

        if(pageStockReturn.isEmpty()){
            return new PageImpl<>(Collections.emptyList());
        }

        List<StockReturnDTO> stockReturnDTOS = pageStockReturn.getContent().stream().map(stockReturn -> StockReturnDTO.builder()
                .registrationDate(stockReturn.getRegistrationDate())
                .purchaseSerial(stockReturn.getPurchase().getSerial())
                .updateDate(stockReturn.getUpdateDate())
                .build()).toList();
        return new PageImpl<>(stockReturnDTOS,pageStockReturn.getPageable(),pageStockReturn.getTotalElements());
    }

    @Override
    public List<StockReturnDTO> listStockReturn(String user) throws InternalErrorExceptions, BadRequestExceptions {
        List<StockReturn> stockReturns;
        Long clientId;
        try {
            clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
            stockReturns = stockReturnRepository.findAllByClientIdAndStatusTrue(clientId);
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(stockReturns.isEmpty()){
            return Collections.emptyList();
        }

        return stockReturns.stream().map(stockReturn -> StockReturnDTO.builder()
                .registrationDate(stockReturn.getRegistrationDate())
                .purchaseSerial(stockReturn.getPurchase().getSerial())
                .updateDate(stockReturn.getUpdateDate())
                .build()).toList();
    }

    @Override
    public List<StockReturnDTO> listStockReturnFalse(String user) throws InternalErrorExceptions, BadRequestExceptions {
        List<StockReturn> stockReturns;
        Long clientId;
        try {
            clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
            stockReturns = stockReturnRepository.findAllByClientIdAndStatusFalse(clientId);
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(stockReturns.isEmpty()){
            return Collections.emptyList();
        }

        return stockReturns.stream().map(stockReturn -> StockReturnDTO.builder()
                .registrationDate(stockReturn.getRegistrationDate())
                .purchaseSerial(stockReturn.getPurchase().getSerial())
                .updateDate(stockReturn.getUpdateDate())
                .build()).toList();
    }
}
