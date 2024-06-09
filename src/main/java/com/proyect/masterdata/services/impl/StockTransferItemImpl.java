package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.StockTransfer;
import com.proyect.masterdata.domain.StockTransferItem;
import com.proyect.masterdata.domain.SupplierProduct;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.StockTransferItemDTO;
import com.proyect.masterdata.dto.request.RequestStockTransferItem;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.StockTransferItemRepository;
import com.proyect.masterdata.repository.StockTransferItemRepositoryCustom;
import com.proyect.masterdata.repository.SupplierProductRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IStockTransferItem;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class StockTransferItemImpl implements IStockTransferItem {
    private final StockTransferItemRepository stockTransferItemRepository;
    private final UserRepository userRepository;
    private final StockTransferItemRepositoryCustom stockTransferItemRepositoryCustom;
    private final SupplierProductRepository supplierProductRepository;
    private final IAudit iAudit;
    @Override
    public StockTransferItem save(RequestStockTransferItem requestStockTransferItem, StockTransfer stockTransfer, SupplierProduct supplierProduct, User user) throws InternalErrorExceptions, BadRequestExceptions {

        try{
            StockTransferItem newStockTransferItem = stockTransferItemRepository.save(StockTransferItem.builder()
                            .stockTransfer(stockTransfer)
                            .stockTransferId(stockTransfer.getId())
                            .client(user.getClient())
                            .clientId(user.getClientId())
                            .quantity(requestStockTransferItem.getQuantity())
                            .supplierProduct(supplierProduct)
                            .supplierProductId(supplierProduct.getId())
                            .tokenUser(user.getUsername())
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .updateDate(new Date(System.currentTimeMillis()))
                    .build());
            iAudit.save("ADD_STOCK_TRANSFER_ITEM","ADD STOCK TRANSFER ITEM "+newStockTransferItem.getSupplierProduct().getSerial()+" FOR STOCK TRANSFER "+newStockTransferItem.getStockTransferId()+".", user.getUsername());
            return newStockTransferItem;
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public CompletableFuture<StockTransferItem> saveAsync(RequestStockTransferItem requestStockTransferItem, StockTransfer stockTransfer, SupplierProduct supplierProduct, User user) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            try{
                StockTransferItem newStockTransferItem = stockTransferItemRepository.save(StockTransferItem.builder()
                        .stockTransfer(stockTransfer)
                        .stockTransferId(stockTransfer.getId())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .quantity(requestStockTransferItem.getQuantity())
                        .supplierProduct(supplierProduct)
                        .supplierProductId(supplierProduct.getId())
                        .tokenUser(user.getUsername())
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .updateDate(new Date(System.currentTimeMillis()))
                        .build());
                iAudit.save("ADD_STOCK_TRANSFER_ITEM","ADD STOCK TRANSFER ITEM "+newStockTransferItem.getSupplierProduct().getSerial()+" FOR STOCK TRANSFER "+newStockTransferItem.getStockTransferId()+".", user.getUsername());
                return newStockTransferItem;
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<Page<StockTransferItemDTO>> list(String user, Long stockTransferId, String supplierProductSerial, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<StockTransferItem> pageStockTransferItem;
            Long clientId;
            Long supplierProductId;

            if(supplierProductSerial != null){
                supplierProductId = supplierProductRepository.findBySerial(supplierProductSerial.toUpperCase()).getId();
            }else{
                supplierProductId = null;
            }

            try{
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                pageStockTransferItem = stockTransferItemRepositoryCustom.searchForStockTransferItem(clientId,stockTransferId,supplierProductId,sort,sortColumn,pageNumber,pageSize);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if(pageStockTransferItem.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }

            List<StockTransferItemDTO> stockTransferItemDTOS = pageStockTransferItem.getContent().stream().map(stockTransferItem -> StockTransferItemDTO.builder()
                    .stockTransferId(stockTransferItem.getStockTransferId())
                    .supplierProductSerial(stockTransferItem.getSupplierProduct().getSerial())
                    .quantity(stockTransferItem.getQuantity())
                    .registrationDate(stockTransferItem.getRegistrationDate())
                    .build()
            ).toList();

            return new PageImpl<>(stockTransferItemDTOS,pageStockTransferItem.getPageable(),pageStockTransferItem.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<StockTransferItemDTO>> listStockTransferItem(String user,Long id) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<StockTransferItem> stockTransferItems;
            Long clientId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                if(id != null){
                    stockTransferItems = stockTransferItemRepository.findAllByClientIdAndStockTransferId(clientId,id);
                }else{
                    stockTransferItems = stockTransferItemRepository.findAllByClientId(clientId);
                }
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if (stockTransferItems.isEmpty()){
                return Collections.emptyList();
            }
            return stockTransferItems.stream().map(stockTransferItem -> StockTransferItemDTO.builder()
                    .serial(stockTransferItem.getStockTransfer().getSerial())
                    .stockTransferId(stockTransferItem.getStockTransferId())
                    .supplierProductSerial(stockTransferItem.getSupplierProduct().getSerial())
                    .quantity(stockTransferItem.getQuantity())
                    .originWarehouse(stockTransferItem.getStockTransfer().getOriginWarehouse().getName())
                    .destinationWarehouse(stockTransferItem.getStockTransfer().getDestinationWarehouse().getName())
                    .registrationDate(stockTransferItem.getRegistrationDate())
                    .build()
            ).toList();
        });
    }
}
