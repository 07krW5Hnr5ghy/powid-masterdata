package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.StockTransfer;
import com.proyect.masterdata.domain.StockTransferItem;
import com.proyect.masterdata.domain.SupplierProduct;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.request.RequestStockTransferItem;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.StockTransferItemRepository;
import com.proyect.masterdata.services.IStockTransferItem;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.util.Date;

@Service
@RequiredArgsConstructor
@Log4j2
public class StockTransferItemImpl implements IStockTransferItem {
    private final StockTransferItemRepository stockTransferItemRepository;
    @Override
    public StockTransferItem save(RequestStockTransferItem requestStockTransferItem, StockTransfer stockTransfer, SupplierProduct supplierProduct, User user) throws InternalErrorExceptions, BadRequestExceptions {

        try{
            return stockTransferItemRepository.save(StockTransferItem.builder()
                            .stockTransfer(stockTransfer)
                            .stockTransferId(stockTransfer.getId())
                            .client(user.getClient())
                            .clientId(user.getClientId())
                            .quantity(requestStockTransferItem.getQuantity())
                            .supplierProduct(supplierProduct)
                            .supplierProductId(supplierProduct.getId())
                            .tokenUser(user.getUsername())
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .updateDate(new Date(System.currentTimeMillis()))
                    .build());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.InternalErrorExceptions);
        }
    }
}
