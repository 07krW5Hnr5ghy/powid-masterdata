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
import com.proyect.masterdata.repository.WarehouseRepository;
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
    private final WarehouseRepository warehouseRepository;

    @Override
    public ResponseSuccess save(List<RequestStockTransaction> stockTransactionDataList, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {

        User user;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        try {
            stockTransactionRepository.saveAll(stockTransactionDataList.stream().map(stockTransaction -> {

                StockTransactionType stockTransactionType = stockTransactionTypeRepository
                        .findByNameAndStatusTrue(stockTransaction.getStockTransactionType().toUpperCase());

                if (stockTransactionType == null) {
                    throw new BadRequestExceptions(Constants.ErrorStockTransactionType);
                }

                SupplierProduct supplierProduct = supplierProductRepository
                        .findBySerialAndStatusTrue(stockTransaction.getSupplierProductSerial());

                if (supplierProduct == null) {
                    throw new BadRequestExceptions(Constants.ErrorSupplier);
                }

                Warehouse warehouse = warehouseRepository
                        .findByNameAndStatusTrue(stockTransaction.getWarehouse().toUpperCase());

                if (warehouse == null) {
                    throw new BadRequestExceptions(Constants.ErrorWarehouse);
                }

                return StockTransaction.builder()
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .quantity(stockTransaction.getQuantity())
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .stockTransactionType(stockTransactionType)
                        .stockTransactionTypeId(stockTransactionType.getId())
                        .supplierProduct(supplierProduct)
                        .supplierProductId(supplierProduct.getId())
                        .tokenUser(user.getUsername())
                        .build();
            }).toList());

            return ResponseSuccess.builder()
                    .message(Constants.register)
                    .code(200)
                    .build();
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }
}
