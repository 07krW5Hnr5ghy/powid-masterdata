package com.proyect.masterdata.services.impl;

import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IAudit;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import com.proyect.masterdata.dto.StockTransactionItemDTO;
import com.proyect.masterdata.dto.request.RequestStockTransactionItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.services.IStockTransactionItem;
import com.proyect.masterdata.utils.Constants;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class StockTransactionItemImpl implements IStockTransactionItem {

    private final UserRepository userRepository;
    private final StockTransactionItemRepository stockTransactionItemRepository;
    private final SupplierProductRepository supplierProductRepository;
    private final StockTransactionItemRepositoryCustom stockTransactionItemRepositoryCustom;
    private final StockTransactionRepository stockTransactionRepository;
    private final IAudit iAudit;
    @Override
    public ResponseSuccess save(StockTransaction stockTransaction,RequestStockTransactionItem requestStockTransactionItem, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        SupplierProduct supplierProduct;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(requestStockTransactionItem.getSupplierProductSerial().toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(supplierProduct == null){
            throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
        }

        try {
            StockTransactionItem newStockTransactionItem = stockTransactionItemRepository.save(StockTransactionItem.builder()
                            .stockTransaction(stockTransaction)
                            .stockTransactionId(stockTransaction.getId())
                            .supplierProduct(supplierProduct)
                            .supplierProductId(supplierProduct.getId())
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .client(user.getClient())
                            .clientId(user.getClientId())
                            .quantity(requestStockTransactionItem.getQuantity())
                            .tokenUser(user.getUsername())
                    .build());
            iAudit.save("ADD_STOCK_TRANSACTION_ITEM","ADD STOCK TRANSACTION ITEM "+newStockTransactionItem.getSupplierProduct().getSerial()+" FOR STOCK TRANSACTION "+newStockTransactionItem.getStockTransactionId()+".",user.getUsername());
            return ResponseSuccess.builder()
                    .message(Constants.register)
                    .code(200)
                    .build();
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public CompletableFuture<ResponseSuccess> saveAsync(StockTransaction stockTransaction, RequestStockTransactionItem requestStockTransactionItem, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            SupplierProduct supplierProduct;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(requestStockTransactionItem.getSupplierProductSerial().toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(supplierProduct == null){
                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
            }

            try {
                StockTransactionItem newStockTransactionItem = stockTransactionItemRepository.save(StockTransactionItem.builder()
                        .stockTransaction(stockTransaction)
                        .stockTransactionId(stockTransaction.getId())
                        .supplierProduct(supplierProduct)
                        .supplierProductId(supplierProduct.getId())
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .quantity(requestStockTransactionItem.getQuantity())
                        .tokenUser(user.getUsername())
                        .build());
                iAudit.save("ADD_STOCK_TRANSACTION_ITEM","ADD STOCK TRANSACTION ITEM "+newStockTransactionItem.getSupplierProduct().getSerial()+" FOR STOCK TRANSACTION "+newStockTransactionItem.getStockTransactionId()+".",user.getUsername());
                return ResponseSuccess.builder()
                        .message(Constants.register)
                        .code(200)
                        .build();
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<Page<StockTransactionItemDTO>> list(String user, String stockTransactionSerial, String supplierProductSerial, String sort, String sortColumn,
                                              Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Long clientId;
            Long stockTransactionId;
            Long supplierProductId;
            Page<StockTransactionItem> stockTransactionPage;

            if (stockTransactionSerial != null) {
                stockTransactionId = stockTransactionRepository.findBySerial(stockTransactionSerial.toUpperCase()).getId();
            } else {
                stockTransactionId = null;
            }

            if(supplierProductSerial != null){
                supplierProductId = supplierProductRepository.findBySerial(supplierProductSerial.toUpperCase()).getId();
            }else{
                supplierProductId = null;
            }

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                stockTransactionPage = stockTransactionItemRepositoryCustom.searchForStockTransactionItem(clientId, stockTransactionId, supplierProductId,
                        sort, sortColumn, pageNumber, pageSize);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (stockTransactionPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }

            List<StockTransactionItemDTO> stockTransactionItemDTOS = stockTransactionPage.getContent().stream()
                    .map(stockTransactionItem -> StockTransactionItemDTO.builder()
                            .quantity(stockTransactionItem.getQuantity())
                            .warehouse(stockTransactionItem.getStockTransaction().getWarehouse().getName())
                            .supplierProduct(stockTransactionItem.getSupplierProduct().getSerial())
                            .serial(stockTransactionItem.getStockTransaction().getSerial())
                            .transactionType(stockTransactionItem.getStockTransaction().getStockTransactionType().getName())
                            .registrationDate(stockTransactionItem.getRegistrationDate())
                            .build())
                    .toList();

            return new PageImpl<>(stockTransactionItemDTOS, stockTransactionPage.getPageable(),
                    stockTransactionPage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<StockTransactionItemDTO>> listStockTransactionItem(String user,Long id) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<StockTransactionItem> stockTransactionItems;
            Long clientId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                if(id != null){
                    stockTransactionItems = stockTransactionItemRepository.findAllByClientIdAndStockTransactionId(clientId,id);
                }else{
                    stockTransactionItems = stockTransactionItemRepository.findAllByClientId(clientId);
                }
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if (stockTransactionItems.isEmpty()){
                return Collections.emptyList();
            }
            return stockTransactionItems.stream()
                    .map(stockTransactionItem -> StockTransactionItemDTO.builder()
                            .quantity(stockTransactionItem.getQuantity())
                            .warehouse(stockTransactionItem.getStockTransaction().getWarehouse().getName())
                            .supplierProduct(stockTransactionItem.getSupplierProduct().getSerial())
                            .serial(stockTransactionItem.getStockTransaction().getSerial())
                            .transactionType(stockTransactionItem.getStockTransaction().getStockTransactionType().getName())
                            .registrationDate(stockTransactionItem.getRegistrationDate())
                            .build())
                    .toList();
        });
    }
}
