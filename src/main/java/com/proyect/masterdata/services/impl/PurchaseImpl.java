package com.proyect.masterdata.services.impl;

import java.util.Collections;
import java.util.Date;
import java.util.List;

import com.proyect.masterdata.dto.response.ResponseDelete;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import com.proyect.masterdata.domain.Purchase;
import com.proyect.masterdata.domain.SupplierProduct;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.domain.Warehouse;
import com.proyect.masterdata.dto.PurchaseDTO;
import com.proyect.masterdata.dto.request.RequestPurchase;
import com.proyect.masterdata.dto.request.RequestStockTransaction;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.PurchaseRepository;
import com.proyect.masterdata.repository.PurchaseRepositoryCustom;
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
    private final PurchaseRepositoryCustom purchaseRepositoryCustom;

    @Override
    public ResponseSuccess save(String serial, List<RequestPurchase> items, String tokenUser)
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
                        .tokenUser(user.getUsername())
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

    @Override
    public Page<PurchaseDTO> list(String serial, String user, String sort, String sortColumn,
            Integer pageNumber, Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions {

        Page<Purchase> pagePurchase;
        Long clientId;

        try {
            clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
            pagePurchase = purchaseRepositoryCustom.searchForPurchase(clientId, serial, sort, sortColumn,
                    pageNumber, pageSize, true);
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.ResultsFound);
        }

        if (pagePurchase.isEmpty()) {
            return new PageImpl<>(Collections.emptyList());
        }

        List<PurchaseDTO> purchaseDTOs = pagePurchase.getContent().stream().map(purchase -> PurchaseDTO.builder()
                .date(purchase.getRegistrationDate())
                .quantity(purchase.getQuantity())
                .serial(purchase.getSerial())
                .supplierProductSerial(purchase.getSupplierProduct().getSerial())
                .unitPrice(purchase.getUnitPrice())
                .build()).toList();

        return new PageImpl<>(purchaseDTOs, pagePurchase.getPageable(), pagePurchase.getTotalElements());
    }

    @Override
    public ResponseDelete delete(String serial, String serialSupplierProduct, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        SupplierProduct supplierProduct;
        Purchase purchase;

        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(serialSupplierProduct.toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(supplierProduct == null){
            throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
        }

        try{
            purchase = purchaseRepository.findBySerialAndSupplierProductId(serial,supplierProduct.getId());
            purchase.setStatus(false);
            purchase.setUpdateDate(new Date(System.currentTimeMillis()));
            purchaseRepository.save(purchase);
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

}
