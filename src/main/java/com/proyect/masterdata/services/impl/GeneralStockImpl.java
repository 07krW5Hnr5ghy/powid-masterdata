package com.proyect.masterdata.services.impl;

import java.util.Date;

import org.springframework.stereotype.Service;

import com.proyect.masterdata.domain.GeneralStock;
import com.proyect.masterdata.domain.SupplierProduct;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.GeneralStockRepository;
import com.proyect.masterdata.repository.SupplierProductRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IGeneralStock;
import com.proyect.masterdata.utils.Constants;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class GeneralStockImpl implements IGeneralStock {

    private final UserRepository userRepository;
    private final SupplierProductRepository supplierProductRepository;
    private final GeneralStockRepository generalStockRepository;

    @Override
    public ResponseSuccess in(String supplierProductSerial, Integer quantity, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        SupplierProduct supplierProduct;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(supplierProductSerial.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (supplierProduct == null) {
            throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
        }

        try {
            GeneralStock generalStock = generalStockRepository.findByClientIdAndSupplierProductId(user.getClientId(),
                    supplierProduct.getId());
            if (generalStock != null) {
                generalStock.setQuantity(generalStock.getQuantity() + quantity);
                generalStock.setUpdateDate(new Date(System.currentTimeMillis()));
                generalStockRepository.save(generalStock);
            } else {
                generalStockRepository.save(GeneralStock.builder()
                        .quantity(quantity)
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .supplierProduct(supplierProduct)
                        .supplierProductId(supplierProduct.getId())
                        .build());
            }

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
    public ResponseSuccess out(String supplierProductSerial, Integer quantity, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        SupplierProduct supplierProduct;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(supplierProductSerial.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (supplierProduct == null) {
            throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
        }

        try {
            GeneralStock generalStock = generalStockRepository.findByClientIdAndSupplierProductId(user.getClientId(),
                    supplierProduct.getId());

            if (generalStock == null) {
                throw new BadRequestExceptions(Constants.ErrorGeneralStock);
            }

            if (quantity > generalStock.getQuantity()) {
                throw new BadRequestExceptions(Constants.ErrorGeneralStockLess);
            }

            generalStock.setQuantity(generalStock.getQuantity() - quantity);

            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

    }

}
