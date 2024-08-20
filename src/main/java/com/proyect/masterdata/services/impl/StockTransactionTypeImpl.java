package com.proyect.masterdata.services.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import com.proyect.masterdata.domain.StockTransaction;
import com.proyect.masterdata.services.IAudit;
import org.springframework.stereotype.Service;

import com.proyect.masterdata.domain.StockTransactionType;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.StockTransactionTypeDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.StockTransactionTypeRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IStockTransactionType;
import com.proyect.masterdata.utils.Constants;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class StockTransactionTypeImpl implements IStockTransactionType {

    private final UserRepository userRepository;
    private final StockTransactionTypeRepository stockTransactionTypeRepository;
    private final IAudit iAudit;
    @Override
    public ResponseSuccess save(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        StockTransactionType stockTransactionType;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            stockTransactionType = stockTransactionTypeRepository.findByName(name.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (stockTransactionType != null) {
            throw new BadRequestExceptions(Constants.ErrorStockTransactionTypeExists);
        }

        try {
            StockTransactionType newStockTransactionType = stockTransactionTypeRepository.save(StockTransactionType.builder()
                    .name(name.toUpperCase())
                    .registrationDate(new Date(System.currentTimeMillis()))
                    .status(true)
                    .tokenUser(tokenUser.toUpperCase())
                    .build());
            iAudit.save("ADD_STOCK_TRANSACTION_TYPE","TIPO DE TRANSACCION "+newStockTransactionType.getName()+" CREADO.",newStockTransactionType.getName(),user.getUsername());
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public CompletableFuture<ResponseSuccess> saveAsync(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            StockTransactionType stockTransactionType;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                stockTransactionType = stockTransactionTypeRepository.findByName(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if (stockTransactionType != null) {
                throw new BadRequestExceptions(Constants.ErrorStockTransactionTypeExists);
            }

            try {
                StockTransactionType newStockTransactionType = stockTransactionTypeRepository.save(StockTransactionType.builder()
                        .name(name.toUpperCase())
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .status(true)
                        .tokenUser(tokenUser.toUpperCase())
                        .build());
                iAudit.save("ADD_STOCK_TRANSACTION_TYPE","TIPO DE TRANSACCION "+newStockTransactionType.getName()+" CREADO.",newStockTransactionType.getName(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }


    @Override
    public CompletableFuture<ResponseDelete> delete(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            StockTransactionType stockTransactionType;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                stockTransactionType = stockTransactionTypeRepository.findByNameAndStatusTrue(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if (stockTransactionType == null) {
                throw new BadRequestExceptions(Constants.ErrorStockTransactionType);
            }

            try {
                stockTransactionType.setUpdateDate(new Date(System.currentTimeMillis()));
                stockTransactionType.setStatus(false);
                stockTransactionType.setTokenUser(user.getUsername());
                stockTransactionTypeRepository.save(stockTransactionType);
                iAudit.save("DELETE_STOCK_TRANSACTION_TYPE","TIPO DE TRANSACCION "+stockTransactionType.getName()+" DESACTIVADO.",stockTransactionType.getName(),user.getUsername());
                return ResponseDelete.builder()
                        .code(200)
                        .message(Constants.delete)
                        .build();
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> activate(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            StockTransactionType stockTransactionType;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                stockTransactionType = stockTransactionTypeRepository.findByNameAndStatusFalse(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if (stockTransactionType == null) {
                throw new BadRequestExceptions(Constants.ErrorStockTransactionType);
            }

            try {
                stockTransactionType.setUpdateDate(new Date(System.currentTimeMillis()));
                stockTransactionType.setStatus(true);
                stockTransactionType.setTokenUser(user.getUsername());
                stockTransactionTypeRepository.save(stockTransactionType);
                iAudit.save("ACTIVATE_STOCK_TRANSACTION_TYPE","TIPO DE TRANSACCION "+stockTransactionType.getName()+" ACTIVADO.",stockTransactionType.getName(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.update)
                        .build();
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<List<StockTransactionTypeDTO>> list() throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<StockTransactionTypeDTO> stockTransactionTypeList;

            try {
                stockTransactionTypeList = stockTransactionTypeRepository.findAllByStatusTrue().stream()
                        .map(stockTransactionType -> StockTransactionTypeDTO.builder()
                                .name(stockTransactionType.getName())
                                .build())
                        .toList();
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (stockTransactionTypeList.isEmpty()) {
                return Collections.emptyList();
            }

            return stockTransactionTypeList;
        });
    }

    @Override
    public CompletableFuture<List<StockTransactionTypeDTO>> listFilter() throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<StockTransactionTypeDTO> stockTransactionTypeList;

            try {
                stockTransactionTypeList = stockTransactionTypeRepository.findAll().stream()
                        .map(stockTransactionType -> StockTransactionTypeDTO.builder()
                                .name(stockTransactionType.getName())
                                .build())
                        .toList();
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (stockTransactionTypeList.isEmpty()) {
                return Collections.emptyList();
            }

            return stockTransactionTypeList;
        });
    }

}
