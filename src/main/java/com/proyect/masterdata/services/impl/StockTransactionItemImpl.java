package com.proyect.masterdata.services.impl;

import java.util.Collections;
import java.util.Date;
import java.util.List;

import com.proyect.masterdata.domain.*;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import com.proyect.masterdata.dto.StockTransactionItemDTO;
import com.proyect.masterdata.dto.request.RequestStockTransactionItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.StockTransactionItemRepository;
import com.proyect.masterdata.repository.StockTransactionItemRepositoryCustom;
import com.proyect.masterdata.repository.StockTransactionTypeRepository;
import com.proyect.masterdata.repository.SupplierProductRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.repository.WarehouseRepository;
import com.proyect.masterdata.services.IStockTransactionItem;
import com.proyect.masterdata.utils.Constants;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class StockTransactionItemImpl implements IStockTransactionItem {

    private final UserRepository userRepository;
    private final StockTransactionItemRepository stockTransactionItemRepository;
    private final StockTransactionTypeRepository stockTransactionTypeRepository;
    private final SupplierProductRepository supplierProductRepository;
    private final WarehouseRepository warehouseRepository;
    private final StockTransactionItemRepositoryCustom stockTransactionItemRepositoryCustom;

    @Override
    public ResponseSuccess save(StockTransaction stockTransaction,RequestStockTransactionItem requestStockTransactionItem, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        SupplierProduct supplierProduct;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(requestStockTransactionItem.getSupplierProductSerial().toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(supplierProduct == null){
            throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
        }

        try {
            stockTransactionItemRepository.save(StockTransactionItem.builder()
                            .stockTransaction(stockTransaction)
                            .stockTransactionId(stockTransaction.getId())
                            .supplierProduct(supplierProduct)
                            .supplierProductId(supplierProduct.getId())
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .client(user.getClient())
                            .clientId(user.getClientId())
                            .quantity(requestStockTransactionItem.getQuantity())
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
    public Page<StockTransactionItemDTO> list(String user, String warehouse, String sort, String sortColumn,
                                              Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        Long clientId;
        Long warehouseId;
        Page<StockTransactionItem> stockTransactionPage;

        if (warehouse != null) {
            warehouseId = warehouseRepository.findByNameAndStatusTrue(warehouse.toUpperCase()).getId();
        } else {
            warehouseId = null;
        }

        try {
            clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
            stockTransactionPage = stockTransactionItemRepositoryCustom.searchForStockTransaction(clientId, warehouseId,
                    sort, sortColumn, pageNumber, pageSize);
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.ResultsFound);
        }

        if (stockTransactionPage.isEmpty()) {
            return new PageImpl<>(Collections.emptyList());
        }

        List<StockTransactionItemDTO> stockTransactionItemDTOS = stockTransactionPage.getContent().stream()
                .map(stockTransactionItem -> StockTransactionItemDTO.builder()
                        .quantity(stockTransactionItem.getQuantity())
                        .warehouse(stockTransactionItem.getStockTransaction().getWarehouse().getName())
                        .supplierProductSerial(stockTransactionItem.getSupplierProduct().getSerial())
                        .stockTransactionType(stockTransactionItem.getStockTransaction().getStockTransactionType().getName())
                        .date(stockTransactionItem.getRegistrationDate())
                        .build())
                .toList();

        return new PageImpl<>(stockTransactionItemDTOS, stockTransactionPage.getPageable(),
                stockTransactionPage.getTotalElements());
    }
}
