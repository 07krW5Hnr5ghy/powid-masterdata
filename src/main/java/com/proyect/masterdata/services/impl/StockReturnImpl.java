package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.PurchaseItem;
import com.proyect.masterdata.domain.StockReturn;
import com.proyect.masterdata.domain.SupplierProduct;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.StockReturnDTO;
import com.proyect.masterdata.dto.request.RequestStockReturn;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IStockReturn;
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
public class StockReturnImpl implements IStockReturn {
    private final UserRepository userRepository;
    private final PurchaseItemRepository purchaseItemRepository;
    private final SupplierProductRepository supplierProductRepository;
    private final StockReturnRepository stockReturnRepository;
    private final StockReturnRepositoryCustom stockReturnRepositoryCustom;
    @Override
    public ResponseSuccess save(List<RequestStockReturn> requestStockReturnList, String purchaseSerial, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        try{
            for(RequestStockReturn requestStockReturn : requestStockReturnList){
                SupplierProduct supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(requestStockReturn.getSupplierProductSerial().toUpperCase());
                if(supplierProduct == null){
                    throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
                }
                PurchaseItem purchaseItem = purchaseItemRepository.findBySerialAndSupplierProductId(purchaseSerial,supplierProduct.getId());
                if(purchaseItem == null){
                    throw new BadRequestExceptions(Constants.ErrorPurchase);
                }
                if(requestStockReturn.getQuantity() > purchaseItem.getQuantity()){
                    throw new BadRequestExceptions(Constants.ErrorStockReturnQuantity);
                }
                StockReturn stockReturn = stockReturnRepository.findByPurchaseIdAndSupplierProductId(purchaseItem.getId(),supplierProduct.getId());
                if(stockReturn != null){
                    throw new BadRequestExceptions(Constants.ErrorStockReturnExists);
                }
                stockReturnRepository.save(StockReturn.builder()
                                .client(user.getClient())
                                .clientId(user.getClientId())
                                .observations(requestStockReturn.getObservations())
                                .status(true)
                                .quantity(requestStockReturn.getQuantity())
                                .supplierProduct(supplierProduct)
                                .supplierProductId(supplierProduct.getId())
                                .registrationDate(new Date(System.currentTimeMillis()))
                                .updateDate(new Date(System.currentTimeMillis()))
                                .purchaseItem(purchaseItem)
                                .purchaseId(purchaseItem.getId())
                                .tokenUser(user.getUsername())
                        .build());
            }
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public Page<StockReturnDTO> list(String purchaseSerial, String user, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        Page<StockReturn> pageStockReturn;
        Long clientId;
        Long purchaseId;

        if(purchaseSerial != null){
            System.out.println(purchaseItemRepository.findBySerial(purchaseSerial.toUpperCase()).getId());
            purchaseId = purchaseItemRepository.findBySerial(purchaseSerial.toUpperCase()).getId();
        }else {
            purchaseId = null;
        }

        try {
            clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
            pageStockReturn = stockReturnRepositoryCustom.searchForStockReturn(purchaseId,clientId,sort,sortColumn,pageNumber,pageSize,true);
        } catch (RuntimeException e){
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.ResultsFound);
        }

        if(pageStockReturn.isEmpty()){
            return new PageImpl<>(Collections.emptyList());
        }

        List<StockReturnDTO> stockReturnDTOS = pageStockReturn.getContent().stream().map(stockReturn -> {
            PurchaseItem purchaseItem = purchaseItemRepository.findBySerialAndSupplierProductId(stockReturn.getPurchaseItem().getSerial(),stockReturn.getSupplierProductId());
            return StockReturnDTO.builder()
                    .purchaseSerial(purchaseItem.getSerial())
                    .supplierProductSerial(stockReturn.getSupplierProduct().getSerial())
                    .registrationDate(stockReturn.getRegistrationDate())
                    .updateDate(stockReturn.getUpdateDate())
                    .quantity(stockReturn.getQuantity())
                    .observations(stockReturn.getObservations())
                    .updateDate(stockReturn.getUpdateDate())
                    .build();
        }).toList();
        return new PageImpl<>(stockReturnDTOS,pageStockReturn.getPageable(),pageStockReturn.getTotalElements());
    }

    @Override
    public Page<StockReturnDTO> listFalse(String purchaseSerial, String user, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        Page<StockReturn> pageStockReturn;
        Long clientId;
        Long purchaseId;

        if(purchaseSerial != null){
            purchaseId = purchaseItemRepository.findBySerial(purchaseSerial.toUpperCase()).getId();
        }else {
            purchaseId = null;
        }

        try {
            clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
            pageStockReturn = stockReturnRepositoryCustom.searchForStockReturn(purchaseId,clientId,sort,sortColumn,pageNumber,pageSize,false);
        } catch (RuntimeException e){
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.ResultsFound);
        }

        if(pageStockReturn.isEmpty()){
            return new PageImpl<>(Collections.emptyList());
        }

        List<StockReturnDTO> stockReturnDTOS = pageStockReturn.getContent().stream().map(stockReturn -> {
            PurchaseItem purchaseItem = purchaseItemRepository.findBySerialAndSupplierProductId(stockReturn.getPurchaseItem().getSerial(),stockReturn.getSupplierProductId());
            return StockReturnDTO.builder()
                    .purchaseSerial(purchaseItem.getSerial())
                    .supplierProductSerial(stockReturn.getSupplierProduct().getSerial())
                    .registrationDate(stockReturn.getRegistrationDate())
                    .updateDate(stockReturn.getUpdateDate())
                    .quantity(stockReturn.getQuantity())
                    .observations(stockReturn.getObservations())
                    .updateDate(stockReturn.getUpdateDate())
                    .build();
        }).toList();
        return new PageImpl<>(stockReturnDTOS,pageStockReturn.getPageable(),pageStockReturn.getTotalElements());
    }
}