package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.StockTransaction;
import com.proyect.masterdata.domain.StockTransactionType;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.domain.Warehouse;
import com.proyect.masterdata.dto.StockTransactionDTO;
import com.proyect.masterdata.dto.request.RequestStockTransactionItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IStockTransaction;
import com.proyect.masterdata.services.IStockTransactionItem;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.time.OffsetDateTime;
import java.util.*;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class StockTransactionImpl implements IStockTransaction {

    private final UserRepository userRepository;
    private final WarehouseRepository warehouseRepository;
    private final StockTransactionRepository stockTransactionRepository;
    private final StockTransactionTypeRepository stockTransactionTypeRepository;
    private final IStockTransactionItem iStockTransactionItem;
    private final StockTransactionRepositoryCustom stockTransactionRepositoryCustom;
    private final IAudit iAudit;
    @Override
    public StockTransaction save(String serial, Warehouse warehouse, List<RequestStockTransactionItem> requestStockTransactionItemList,String stockTransactionType, User user) throws BadRequestExceptions, InternalErrorExceptions {
        StockTransaction stockTransaction;
        StockTransactionType stockTransactionTypeData;

        try{
            stockTransaction = stockTransactionRepository.findBySerial(serial.toUpperCase());
            stockTransactionTypeData = stockTransactionTypeRepository.findByNameAndStatusTrue(stockTransactionType.toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(stockTransaction != null){
            throw new BadRequestExceptions(Constants.ErrorStockTransactionExists);
        }

        if(stockTransactionTypeData == null){
            throw new BadRequestExceptions(Constants.ErrorStockTransactionType);
        }

        try{

            StockTransaction newStockTransaction = stockTransactionRepository.save(StockTransaction.builder()
                            .serial(serial.toUpperCase())
                            .stockTransactionType(stockTransactionTypeData)
                            .stockTransactionTypeId(stockTransactionTypeData.getId())
                            .warehouse(warehouse)
                            .warehouseId(warehouse.getId())
                            .client(user.getClient())
                            .clientId(user.getClientId())
                            .registrationDate(OffsetDateTime.now())
                            .user(user).userId(user.getId())
                    .build());

            for(RequestStockTransactionItem requestStockTransactionItem : requestStockTransactionItemList){
                iStockTransactionItem.save(newStockTransaction,requestStockTransactionItem,user.getUsername());
            }
            iAudit.save("ADD_STOCK_TRANSACTION","TRANSACCION DE STOCK "+newStockTransaction.getSerial()+" CREADO.",newStockTransaction.getSerial(),user.getUsername());
            return newStockTransaction;
        }catch (RuntimeException e){
            e.printStackTrace();
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public CompletableFuture<StockTransaction> saveAsync(String serial, Warehouse warehouse, List<RequestStockTransactionItem> requestStockTransactionItemList, String stockTransactionType, User user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            StockTransaction stockTransaction;
            StockTransactionType stockTransactionTypeData;

            try{
                stockTransaction = stockTransactionRepository.findBySerial(serial.toUpperCase());
                stockTransactionTypeData = stockTransactionTypeRepository.findByNameAndStatusTrue(stockTransactionType.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(stockTransaction != null){
                throw new BadRequestExceptions(Constants.ErrorStockTransactionExists);
            }

            if(stockTransactionTypeData == null){
                throw new BadRequestExceptions(Constants.ErrorStockTransactionType);
            }

            try{

                StockTransaction newStockTransaction = stockTransactionRepository.save(StockTransaction.builder()
                        .serial(serial.toUpperCase())
                        .stockTransactionType(stockTransactionTypeData)
                        .stockTransactionTypeId(stockTransactionTypeData.getId())
                        .warehouse(warehouse)
                        .warehouseId(warehouse.getId())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .registrationDate(OffsetDateTime.now())
                        .user(user).userId(user.getId())
                        .build());

                for(RequestStockTransactionItem requestStockTransactionItem : requestStockTransactionItemList){
                    iStockTransactionItem.save(newStockTransaction,requestStockTransactionItem,user.getUsername());
                }
                iAudit.save("ADD_STOCK_TRANSACTION","TRANSACCION DE STOCK "+newStockTransaction.getSerial()+" CREADO.",newStockTransaction.getSerial(),user.getUsername());
                return newStockTransaction;
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public StockTransaction addStockTransaction(String serial, Warehouse warehouse, List<RequestStockTransactionItem> requestStockTransactionItemList, String stockTransactionType, User user) throws BadRequestExceptions, InternalErrorExceptions {
        StockTransaction stockTransaction;
        StockTransactionType stockTransactionTypeData;

        try{
            stockTransaction = stockTransactionRepository.findBySerial(serial.toUpperCase());
            stockTransactionTypeData = stockTransactionTypeRepository.findByNameAndStatusTrue(stockTransactionType.toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(stockTransaction != null){
            throw new BadRequestExceptions(Constants.ErrorStockTransactionExists);
        }

        if(stockTransactionTypeData == null){
            throw new BadRequestExceptions(Constants.ErrorStockTransactionType);
        }

        try{

            StockTransaction newStockTransaction = stockTransactionRepository.save(StockTransaction.builder()
                    .serial(serial.toUpperCase())
                    .stockTransactionType(stockTransactionTypeData)
                    .stockTransactionTypeId(stockTransactionTypeData.getId())
                    .warehouse(warehouse)
                    .warehouseId(warehouse.getId())
                    .client(user.getClient())
                    .clientId(user.getClientId())
                    .registrationDate(OffsetDateTime.now())
                    .user(user).userId(user.getId())
                    .build());

            for(RequestStockTransactionItem requestStockTransactionItem : requestStockTransactionItemList){
                iStockTransactionItem.save(newStockTransaction,requestStockTransactionItem,user.getUsername());
            }
            iAudit.save("ADD_STOCK_TRANSACTION","TRANSACCION DE STOCK "+newStockTransaction.getSerial()+" CREADO.",newStockTransaction.getSerial(),user.getUsername());
            return newStockTransaction;
        }catch (RuntimeException e){
            e.printStackTrace();
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public CompletableFuture<Page<StockTransactionDTO>> list(
            String user,
            List<String> serials,
            List<String> warehouses,
            List<String> stockTransactionTypes,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<StockTransaction> pageStockTransaction;
            UUID clientId;
            List<String> serialsUppercase;
            List<UUID> warehouseIds;
            List<UUID> stockTransactionTypeIds;

            if(serials != null && !serials.isEmpty()){
                serialsUppercase = serials.stream().map(String::toUpperCase).toList();
            }else{
                serialsUppercase = new ArrayList<>();
            }

            if(warehouses != null && !warehouses.isEmpty()){
                warehouseIds = warehouseRepository.findByNameIn(
                        warehouses.stream().map(String::toUpperCase).toList()
                ).stream().map(Warehouse::getId).toList();
            }else {
                warehouseIds = new ArrayList<>();
            }

            if(stockTransactionTypes != null && !stockTransactionTypes.isEmpty()){
                stockTransactionTypeIds = stockTransactionTypeRepository.findByNameIn(
                        stockTransactionTypes.stream().map(String::toUpperCase).toList()
                ).stream().map(StockTransactionType::getId).toList();
            }else {
                stockTransactionTypeIds = new ArrayList<>();
            }

            try{
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                pageStockTransaction = stockTransactionRepositoryCustom.searchForStockTransaction(
                        clientId,
                        serialsUppercase,
                        warehouseIds,
                        stockTransactionTypeIds,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if(pageStockTransaction.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }

            List<StockTransactionDTO> stockTransactionDTOS = pageStockTransaction.getContent().stream().map(stockTransaction -> StockTransactionDTO.builder()
                    .serial(stockTransaction.getSerial())
                    .warehouse(stockTransaction.getWarehouse().getName())
                    .transactionType(stockTransaction.getStockTransactionType().getName())
                    .registrationDate(stockTransaction.getRegistrationDate())
                    .user(stockTransaction.getUser().getUsername())
                    .build()).toList();

            return new PageImpl<>(stockTransactionDTOS,pageStockTransaction.getPageable(),pageStockTransaction.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<StockTransactionDTO>> listStockTransaction(String user) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<StockTransaction> stockTransactions;
            UUID clientId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                stockTransactions = stockTransactionRepository.findAllByClientId(clientId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if (stockTransactions.isEmpty()){
                return Collections.emptyList();
            }
            return stockTransactions.stream().map(stockTransaction -> StockTransactionDTO.builder()
                    .serial(stockTransaction.getSerial())
                    .warehouse(stockTransaction.getWarehouse().getName())
                    .transactionType(stockTransaction.getStockTransactionType().getName())
                    .registrationDate(stockTransaction.getRegistrationDate())
                    .user(stockTransaction.getUser().getUsername())
                    .build()).toList();
        });
    }
}
