package com.proyect.masterdata.services.impl;

import java.time.OffsetDateTime;
import java.util.*;
import java.util.concurrent.CompletableFuture;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IUtil;
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
    private final StockTransactionItemRepositoryCustom stockTransactionItemRepositoryCustom;
    private final StockTransactionRepository stockTransactionRepository;
    private final IAudit iAudit;
    private final WarehouseRepository warehouseRepository;
    private final StockTransactionTypeRepository stockTransactionTypeRepository;
    private final IUtil iUtil;
    private final ProductRepository productRepository;
    @Override
    public ResponseSuccess save(StockTransaction stockTransaction,RequestStockTransactionItem requestStockTransactionItem, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        Product product;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            product = productRepository.findByIdAndStatusTrue(requestStockTransactionItem.getProductId());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(product == null){
            throw new BadRequestExceptions(Constants.ErrorProduct);
        }

        try {
            StockTransactionItem newStockTransactionItem = stockTransactionItemRepository.save(StockTransactionItem.builder()
                            .stockTransaction(stockTransaction)
                            .stockTransactionId(stockTransaction.getId())
                            .registrationDate(OffsetDateTime.now())
                            .client(user.getClient())
                            .clientId(user.getClientId())
                            .quantity(requestStockTransactionItem.getQuantity())
                            .user(user).userId(user.getId())
                    .build());
            iAudit.save(
                    "ADD_STOCK_TRANSACTION_ITEM",
                    "PRODUCTO DE INVENTARIO "+
                            iUtil.buildProductSku(newStockTransactionItem.getProduct())+
                            " PARA TRANSACCION DE STOCK "+
                            newStockTransactionItem.getStockTransaction().getSerial()+
                            " CREADO.",
                    newStockTransactionItem.getStockTransaction().getSerial(),
                    user.getUsername());
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
            Product product;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                product = productRepository.findByIdAndStatusTrue(requestStockTransactionItem.getProductId());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(product == null){
                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
            }

            try {
                StockTransactionItem newStockTransactionItem = stockTransactionItemRepository.save(StockTransactionItem.builder()
                        .stockTransaction(stockTransaction)
                        .stockTransactionId(stockTransaction.getId())
                        .product(product)
                        .productId(product.getId())
                        .registrationDate(OffsetDateTime.now())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .quantity(requestStockTransactionItem.getQuantity())
                        .user(user).userId(user.getId())
                        .build());
                iAudit.save(
                        "ADD_STOCK_TRANSACTION_ITEM",
                        "PRODUCTO DE INVENTARIO "+
                                iUtil.buildProductSku(newStockTransactionItem.getProduct())+
                                " PARA TRANSACCION DE STOCK "+
                                newStockTransactionItem.getStockTransaction().getSerial()+
                                " CREADO.",
                        newStockTransactionItem.getStockTransaction().getSerial(),user.getUsername());
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
    public CompletableFuture<Page<StockTransactionItemDTO>> list(
            String user,
            List<String> stockTransactions,
            List<UUID> supplierProductIds,
            List<String> warehouses,
            List<String> stockTransactionTypes,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            UUID clientId;
            List<UUID> stockTransactionIds;
            List<UUID> warehouseIds;
            List<UUID> stockTransactionTypeIds;
            Page<StockTransactionItem> stockTransactionItemPage;

            if (stockTransactions != null && !stockTransactions.isEmpty()) {
                stockTransactionIds = stockTransactionRepository.findBySerialIn(
                        stockTransactions.stream().map(String::toUpperCase).toList()
                ).stream().map(StockTransaction::getId).toList();
            } else {
                stockTransactionIds = new ArrayList<>();
            }

            if(warehouses!=null && !warehouses.isEmpty()){
                warehouseIds = warehouseRepository
                        .findByNameIn(
                                warehouses.stream().map(String::toUpperCase).toList()
                        ).stream().map(Warehouse::getId).toList();
            }else{
                warehouseIds = new ArrayList<>();
            }

            if(stockTransactionTypes!=null&&!stockTransactionTypes.isEmpty()){
                stockTransactionTypeIds = stockTransactionTypeRepository.findByNameIn(
                        stockTransactionTypes.stream().map(String::toUpperCase).toList()
                ).stream().map(StockTransactionType::getId).toList();
            }else{
                stockTransactionTypeIds = new ArrayList<>();
            }

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                stockTransactionItemPage = stockTransactionItemRepositoryCustom.searchForStockTransactionItem(
                        clientId,
                        stockTransactionIds,
                        supplierProductIds,
                        warehouseIds,
                        stockTransactionTypeIds,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize);
            } catch (RuntimeException e) {
                e.printStackTrace();
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (stockTransactionItemPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }

            List<StockTransactionItemDTO> stockTransactionItemDTOS = stockTransactionItemPage.getContent().stream()
                    .map(stockTransactionItem -> StockTransactionItemDTO.builder()
                            .quantity(stockTransactionItem.getQuantity())
                            .warehouse(stockTransactionItem.getStockTransaction().getWarehouse().getName())
                            .supplierProduct(iUtil.buildProductSku(stockTransactionItem.getProduct()))
                            .serial(stockTransactionItem.getStockTransaction().getSerial())
                            .transactionType(stockTransactionItem.getStockTransaction().getStockTransactionType().getName())
                            .registrationDate(stockTransactionItem.getRegistrationDate())
                            .user(stockTransactionItem.getUser().getUsername())
                            .build())
                    .toList();

            return new PageImpl<>(
                    stockTransactionItemDTOS,
                    stockTransactionItemPage.getPageable(),
                    stockTransactionItemPage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<StockTransactionItemDTO>> listStockTransactionItem(String user,UUID id) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<StockTransactionItem> stockTransactionItems;
            UUID clientId;
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
                            .supplierProduct(iUtil.buildProductSku(stockTransactionItem.getProduct()))
                            .serial(stockTransactionItem.getStockTransaction().getSerial())
                            .transactionType(stockTransactionItem.getStockTransaction().getStockTransactionType().getName())
                            .registrationDate(stockTransactionItem.getRegistrationDate())
                            .user(stockTransactionItem.getUser().getUsername())
                            .build())
                    .toList();
        });
    }
}
