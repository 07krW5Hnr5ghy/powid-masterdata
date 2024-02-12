package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.StockTransaction;
import com.proyect.masterdata.domain.StockTransactionType;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.domain.Warehouse;
import com.proyect.masterdata.dto.request.RequestStockTransactionItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.StockTransactionRepository;
import com.proyect.masterdata.repository.StockTransactionTypeRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.repository.WarehouseRepository;
import com.proyect.masterdata.services.IStockTransaction;
import com.proyect.masterdata.services.IStockTransactionItem;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

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
}
