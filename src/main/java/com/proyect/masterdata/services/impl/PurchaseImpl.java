package com.proyect.masterdata.services.impl;

import java.util.Date;
import java.util.List;

import org.springframework.stereotype.Service;

import com.proyect.masterdata.domain.Purchase;
import com.proyect.masterdata.domain.SupplierProduct;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.domain.Warehouse;
import com.proyect.masterdata.dto.request.RequestPurchase;
import com.proyect.masterdata.dto.request.RequestStockTransaction;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.PurchaseRepository;
import com.proyect.masterdata.repository.SupplierProductRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.repository.WarehouseRepository;
import com.proyect.masterdata.services.IPurchase;
import com.proyect.masterdata.utils.Constants;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class PurchaseImpl implements IPurchase {

    private final UserRepository userRepository;
    private final PurchaseRepository purchaseRepository;
    private final WarehouseRepository warehouseRepository;
    private final SupplierProductRepository supplierProductRepository;

    @Override
    public ResponseSuccess save(String serial, String warehouse, List<RequestPurchase> items, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        Warehouse warehouseData;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            warehouseData = warehouseRepository.findByNameAndStatusTrue(warehouse.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (warehouseData == null) {
            throw new BadRequestExceptions(Constants.ErrorWarehouse);
        }

        try {
            purchaseRepository.saveAll(items.stream().map(purchaseItem -> {

                SupplierProduct supplierProduct = supplierProductRepository
                        .findBySerialAndStatusTrue(purchaseItem.getSupplierProductSerial());

                if (supplierProduct == null) {
                    throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
                }

                Purchase purchase = purchaseRepository.findBySerialAndSupplierProductId(serial.toUpperCase(),
                        supplierProduct.getId());

                if (purchase != null) {
                    throw new BadRequestExceptions(Constants.ErrorPurchaseExists);
                }

                return Purchase.builder()
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .quantity(purchaseItem.getQuantity())
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .serial(serial.toUpperCase())
                        .status(true)
                        .supplierProduct(supplierProduct)
                        .supplierProductId(supplierProduct.getId())
                        .unitPrice(purchaseItem.getUnitPrice())
                        .tokenUser(user.getName())
                        .build();
            }).toList());

            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e) {
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

}
