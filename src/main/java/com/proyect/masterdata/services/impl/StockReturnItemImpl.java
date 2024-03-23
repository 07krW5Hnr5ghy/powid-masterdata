package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.StockReturnDTO;
import com.proyect.masterdata.dto.StockReturnItemDTO;
import com.proyect.masterdata.dto.request.RequestStockReturnItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IStockReturnItem;
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
public class StockReturnItemImpl implements IStockReturnItem {
    private final UserRepository userRepository;
    private final PurchaseRepository purchaseRepository;
    private final PurchaseItemRepository purchaseItemRepository;
    private final SupplierProductRepository supplierProductRepository;
    private final StockReturnItemRepository stockReturnItemRepository;
    private final StockReturnItemRepositoryCustom stockReturnItemRepositoryCustom;
    @Override
    public StockReturnItem save(StockReturn stockReturn,PurchaseItem purchaseItem,RequestStockReturnItem requestStockReturnItem, User user) throws InternalErrorExceptions, BadRequestExceptions {

        try{
            return stockReturnItemRepository.save(StockReturnItem.builder()
                            .purchaseItem(purchaseItem)
                            .purchaseItemId(purchaseItem.getId())
                            .tokenUser(user.getUsername())
                            .quantity(requestStockReturnItem.getQuantity())
                            .supplierProduct(purchaseItem.getSupplierProduct())
                            .supplierProductId(purchaseItem.getSupplierProductId())
                            .client(user.getClient())
                            .clientId(user.getClientId())
                            .purchase(purchaseItem.getPurchase())
                            .purchaseId(purchaseItem.getPurchaseId())
                            .stockReturn(stockReturn)
                            .stockReturnId(stockReturn.getId())
                            .observations(requestStockReturnItem.getObservations())
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .status(true)
                    .build());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public Page<StockReturnItemDTO> list(String purchaseSerial, String user, String supplierProductSerial, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        Page<StockReturnItem> pageStockReturn;
        Long clientId;
        Long purchaseId;
        Long supplierProductId;

        if(purchaseSerial != null){
            purchaseId = purchaseRepository.findBySerial(purchaseSerial.toUpperCase()).getId();
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
            pageStockReturn = stockReturnItemRepositoryCustom.searchForStockReturnItem(purchaseId,clientId,supplierProductId,sort,sortColumn,pageNumber,pageSize);
        } catch (RuntimeException e){
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.ResultsFound);
        }

        if(pageStockReturn.isEmpty()){
            return new PageImpl<>(Collections.emptyList());
        }

        List<StockReturnItemDTO> stockReturnDTOS = pageStockReturn.getContent().stream().map(stockReturnItem -> StockReturnItemDTO.builder()
                    .purchaseSerial(stockReturnItem.getPurchase().getSerial())
                    .supplierProductSerial(stockReturnItem.getSupplierProduct().getSerial())
                    .registrationDate(stockReturnItem.getRegistrationDate())
                    .quantity(stockReturnItem.getQuantity())
                    .observations(stockReturnItem.getObservations())
                    .build()
        ).toList();
        return new PageImpl<>(stockReturnDTOS,pageStockReturn.getPageable(),pageStockReturn.getTotalElements());
    }

    @Override
    public List<StockReturnItemDTO> listStockReturnItem(String user) throws InternalErrorExceptions, BadRequestExceptions {
        List<StockReturnItem> stockReturnItems;
        Long clientId;
        try {
            clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
            stockReturnItems = stockReturnItemRepository.findAllByClientIdAndStatusTrue(clientId);
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if(stockReturnItems.isEmpty()){
            return Collections.emptyList();
        }
        return stockReturnItems.stream().map(stockReturnItem -> StockReturnItemDTO.builder()
                .purchaseSerial(stockReturnItem.getPurchase().getSerial())
                .supplierProductSerial(stockReturnItem.getSupplierProduct().getSerial())
                .registrationDate(stockReturnItem.getRegistrationDate())
                .quantity(stockReturnItem.getQuantity())
                .observations(stockReturnItem.getObservations())
                .build()
        ).toList();
    }

    @Override
    public List<StockReturnItemDTO> listStockReturnItemFalse(String user) throws InternalErrorExceptions, BadRequestExceptions {
        List<StockReturnItem> stockReturnItems;
        Long clientId;
        try {
            clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
            stockReturnItems = stockReturnItemRepository.findAllByClientIdAndStatusFalse(clientId);
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if(stockReturnItems.isEmpty()){
            return Collections.emptyList();
        }
        return stockReturnItems.stream().map(stockReturnItem -> StockReturnItemDTO.builder()
                .purchaseSerial(stockReturnItem.getPurchase().getSerial())
                .supplierProductSerial(stockReturnItem.getSupplierProduct().getSerial())
                .registrationDate(stockReturnItem.getRegistrationDate())
                .quantity(stockReturnItem.getQuantity())
                .observations(stockReturnItem.getObservations())
                .build()
        ).toList();
    }

}