package com.proyect.masterdata.services.impl;

import java.util.Collections;
import java.util.Date;
import java.util.List;

import com.proyect.masterdata.domain.Purchase;
import com.proyect.masterdata.domain.PurchaseItem;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.repository.*;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import com.proyect.masterdata.domain.SupplierProduct;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.PurchaseItemDTO;
import com.proyect.masterdata.dto.request.RequestPurchaseItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.services.IPurchaseItem;
import com.proyect.masterdata.utils.Constants;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class PurchaseItemImpl implements IPurchaseItem {

    private final UserRepository userRepository;
    private final PurchaseRepository purchaseRepository;
    private final PurchaseItemRepository purchaseItemRepository;
    private final SupplierProductRepository supplierProductRepository;
    private final PurchaseItemRepositoryCustom purchaseItemRepositoryCustom;

    @Override
    public ResponseSuccess save(Long purchaseId, RequestPurchaseItem requestPurchaseItem, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        Purchase purchase;
        SupplierProduct supplierProduct;
        PurchaseItem purchaseItem;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            purchase = purchaseRepository.findById(purchaseId).orElse(null);
            supplierProduct = supplierProductRepository
                    .findBySerialAndStatusTrue(requestPurchaseItem.getSupplierProductSerial().toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(purchase == null){
            throw new BadRequestExceptions(Constants.ErrorPurchase);
        }else{
            purchaseItem = purchaseItemRepository.findByPurchaseIdAndSupplierProductId(purchase.getId(),
                    supplierProduct.getId());
        }

        if (supplierProduct == null) {
            throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
        }

        if (purchaseItem != null) {
            throw new BadRequestExceptions(Constants.ErrorPurchaseExists);
        }

        try {

            purchaseItemRepository.save(PurchaseItem.builder()
                    .client(user.getClient())
                    .clientId(user.getClientId())
                    .quantity(requestPurchaseItem.getQuantity())
                    .registrationDate(new Date(System.currentTimeMillis()))
                    .purchase(purchase)
                    .purchaseId(purchase.getId())
                    .status(true)
                    .supplierProduct(supplierProduct)
                    .supplierProductId(supplierProduct.getId())
                    .unitPrice(requestPurchaseItem.getUnitPrice())
                    .tokenUser(user.getUsername())
                    .build());

            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e) {
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public Page<PurchaseItemDTO> list(String serial, String user, String supplierProductSerial, String sort, String sortColumn,
                                      Integer pageNumber, Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions {

        Page<PurchaseItem> pagePurchase;
        Long clientId;
        Long supplierProductId;
        Long purchaseId;

        if(serial != null){
            purchaseId = purchaseRepository.findBySerial(serial.toUpperCase()).getId();
        }else {
            purchaseId = null;
        }

        if(supplierProductSerial != null){
            supplierProductId = supplierProductRepository.findBySerial(supplierProductSerial.toUpperCase()).getId();
        }else {
            supplierProductId = null;
        }

        try {
            clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
            pagePurchase = purchaseItemRepositoryCustom.searchForPurchaseItem(clientId, purchaseId,supplierProductId, sort, sortColumn,
                    pageNumber, pageSize, true);
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.ResultsFound);
        }

        if (pagePurchase.isEmpty()) {
            return new PageImpl<>(Collections.emptyList());
        }

        List<PurchaseItemDTO> purchaseItemDTOS = pagePurchase.getContent().stream().map(purchaseItem -> PurchaseItemDTO.builder()
                .date(purchaseItem.getRegistrationDate())
                .quantity(purchaseItem.getQuantity())
                .serial(purchaseItem.getPurchase().getSerial())
                .supplierProductSerial(purchaseItem.getSupplierProduct().getSerial())
                .unitPrice(purchaseItem.getUnitPrice())
                .build()).toList();

        return new PageImpl<>(purchaseItemDTOS, pagePurchase.getPageable(), pagePurchase.getTotalElements());
    }

    @Override
    public ResponseDelete delete(String purchaseSerial, String serialSupplierProduct, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        SupplierProduct supplierProduct;
        Purchase purchase;
        PurchaseItem purchaseItem;

        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            purchase = purchaseRepository.findBySerial(purchaseSerial.toUpperCase());
            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(serialSupplierProduct.toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(purchase == null){
            throw new BadRequestExceptions(Constants.ErrorPurchase);
        }

        if(supplierProduct == null){
            throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
        }


        try{
            purchaseItem = purchaseItemRepository.findByPurchaseIdAndSupplierProductId(purchase.getId(),supplierProduct.getId());
            purchaseItem.setStatus(false);
            purchaseItem.setUpdateDate(new Date(System.currentTimeMillis()));
            purchaseItemRepository.save(purchaseItem);
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
