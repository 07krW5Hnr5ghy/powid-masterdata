package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.request.RequestStockReplenishmentItem;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.StockReplenishmentItemRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IStockReplenishmentItem;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
@Log4j2
public class StockReplenishmentItemImpl implements IStockReplenishmentItem {
    private final StockReplenishmentItemRepository stockReplenishmentItemRepository;
    @Override
    public StockReplenishmentItem save(OrderItem orderItem, RequestStockReplenishmentItem requestStockReplenishmentItem, User user, StockReplenishment stockReplenishment) throws InternalErrorExceptions, BadRequestExceptions {
        try{
            return stockReplenishmentItemRepository.save(StockReplenishmentItem.builder()
                            .stockReplenishment(stockReplenishment)
                            .stockReplenishmentId(stockReplenishment.getId())
                            .ordering(orderItem.getOrdering())
                            .orderId(orderItem.getOrderId())
                            .tokenUser(user.getUsername())
                            .product(orderItem.getProduct())
                            .productId(orderItem.getProductId())
                            .quantity(requestStockReplenishmentItem.getQuantity())
                            .client(user.getClient())
                            .clientId(user.getClientId())
                    .build());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }
}
