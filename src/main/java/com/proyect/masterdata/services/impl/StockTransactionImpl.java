package com.proyect.masterdata.services.impl;

import java.util.Date;
import java.util.List;

import org.springframework.stereotype.Service;

import com.proyect.masterdata.domain.StockTransactionType;
import com.proyect.masterdata.domain.SupplierProduct;
import com.proyect.masterdata.domain.StockTransaction;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.domain.Warehouse;
import com.proyect.masterdata.dto.request.RequestStockTransaction;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.StockTransactionRepository;
import com.proyect.masterdata.repository.StockTransactionTypeRepository;
import com.proyect.masterdata.repository.SupplierProductRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IStockTransaction;
import com.proyect.masterdata.utils.Constants;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class StockTransactionImpl implements IStockTransaction {

    private final UserRepository userRepository;
    private final StockTransactionRepository stockTransactionRepository;
    private final StockTransactionTypeRepository stockTransactionTypeRepository;
    private final SupplierProductRepository supplierProductRepository;

    @Override
    public ResponseSuccess save(RequestStockTransaction stockTransactionData, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        StockTransactionType stockTransactionType;
        SupplierProduct supplierProduct;
        Warehouse warehouse;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            stockTransactionType = stockTransactionTypeRepository
                    .findByNameAndStatusTrue(stockTransactionData.getStockTransactionType().toUpperCase());
            supplierProduct = supplierProductRepository
                    .findBySerialAndStatusTrue(stockTransactionData.getSupplierProductSerial());

        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        try {
            stockTransactionRepository.save(StockTransaction.builder()
                    .client(user.getClient())
                    .clientId(user.getClientId())
                    .quantity(stockTransactionData.getQuantity())
                    .registrationDate(new Date(System.currentTimeMillis()))
                    .stockTransactionType(stockTransactionType)
                    .stockTransactionTypeId(stockTransactionType.getId())
                    .supplierProduct(supplierProduct)
                    .supplierProductId(supplierProduct.getId())
                    .tokenUser(user.getUsername())
                    .build());

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
    public ResponseSuccess saveAll(List<RequestStockTransaction> stockTransactionDataList, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'saveAll'");
    }

}
