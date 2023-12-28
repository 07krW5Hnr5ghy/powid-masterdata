package com.proyect.masterdata.services.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;

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
            stockTransactionTypeRepository.save(StockTransactionType.builder()
                    .name(name.toUpperCase())
                    .registrationDate(new Date(System.currentTimeMillis()))
                    .status(true)
                    .tokenUser(tokenUser.toUpperCase())
                    .build());

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
    public ResponseSuccess saveAll(List<String> names, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        List<StockTransactionType> stockTransactionTypes;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            stockTransactionTypes = stockTransactionTypeRepository.findByNameIn(
                    names.stream().map(stockTransactionType -> stockTransactionType.toUpperCase()).toList());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (!stockTransactionTypes.isEmpty()) {
            throw new BadRequestExceptions(Constants.ErrorStockTransactionTypeExists);
        }

        try {
            stockTransactionTypeRepository.saveAll(names.stream().map(name -> StockTransactionType.builder()
                    .name(name.toUpperCase())
                    .registrationDate(new Date(System.currentTimeMillis()))
                    .status(true)
                    .tokenUser(tokenUser.toUpperCase())
                    .build()).toList());

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
    public ResponseDelete delete(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
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

            stockTransactionTypeRepository.save(stockTransactionType);

            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public List<StockTransactionTypeDTO> list() throws BadRequestExceptions {
        List<StockTransactionTypeDTO> stockTransactionTypeList = new ArrayList<>();

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
    }

}
