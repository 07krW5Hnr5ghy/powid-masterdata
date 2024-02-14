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
import com.proyect.masterdata.services.IStockTransaction;
import com.proyect.masterdata.services.IStockTransactionItem;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.Date;
import java.util.List;

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
    @Override
    public StockTransaction save(String serial, String warehouse, List<RequestStockTransactionItem> requestStockTransactionItemList,String stockTransactionType, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        User user;
        Warehouse warehouseData;
        StockTransaction stockTransaction;
        StockTransactionType stockTransactionTypeData;

        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            warehouseData = warehouseRepository.findByNameAndStatusTrue(warehouse.toUpperCase());
            stockTransaction = stockTransactionRepository.findBySerial(serial.toUpperCase());
            stockTransactionTypeData = stockTransactionTypeRepository.findByNameAndStatusTrue(stockTransactionType.toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(warehouseData == null){
            throw new BadRequestExceptions(Constants.ErrorWarehouse);
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
                            .warehouse(warehouseData)
                            .warehouseId(warehouseData.getId())
                            .client(user.getClient())
                            .clientId(user.getClientId())
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .tokenUser(user.getUsername())
                    .build());

            for(RequestStockTransactionItem requestStockTransactionItem : requestStockTransactionItemList){
                iStockTransactionItem.save(newStockTransaction,requestStockTransactionItem,user.getUsername());
            }

            return newStockTransaction;
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public Page<StockTransactionDTO> list(String user, String serial, String warehouse, String stockTransactionType, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        Page<StockTransaction> pageStockTransaction;
        Long clientId;
        Long warehouseId;
        String stockTransactionSerial;
        Long stockTransactionTypeId;

        if(serial != null){
            stockTransactionSerial = serial.toUpperCase();
        }else{
            stockTransactionSerial = null;
        }

        if(warehouse != null){
            warehouseId = warehouseRepository.findByName(warehouse.toUpperCase()).getId();
        }else {
            warehouseId = null;
        }

        if(stockTransactionType != null){
            stockTransactionTypeId = stockTransactionTypeRepository.findByName(stockTransactionType.toUpperCase()).getId();
        }else {
            stockTransactionTypeId = null;
        }

        try{
            clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
            pageStockTransaction = stockTransactionRepositoryCustom.searchForStockTransaction(clientId,stockTransactionSerial,warehouseId,stockTransactionTypeId,sort,sortColumn,pageNumber,pageSize);
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
                .stockTransactionType(stockTransaction.getStockTransactionType().getName())
                .registrationDate(stockTransaction.getRegistrationDate())
                .build()).toList();

        return new PageImpl<>(stockTransactionDTOS,pageStockTransaction.getPageable(),pageStockTransaction.getTotalElements());
    }
}
