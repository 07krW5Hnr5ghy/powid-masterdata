package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.OrderItem;
import com.proyect.masterdata.domain.StockReplenishment;
import com.proyect.masterdata.domain.StockReplenishmentItem;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.StockReplenishmentItemDTO;
import com.proyect.masterdata.dto.request.RequestStockReplenishmentItem;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.ProductRepository;
import com.proyect.masterdata.repository.StockReplenishmentItemRepository;
import com.proyect.masterdata.repository.StockReplenishmentItemRepositoryCustom;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IStockReplenishmentItem;
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
public class StockReplenishmentItemImpl implements IStockReplenishmentItem {
    private final StockReplenishmentItemRepository stockReplenishmentItemRepository;
    private final StockReplenishmentItemRepositoryCustom stockReplenishmentItemRepositoryCustom;
    private final UserRepository userRepository;
    private final ProductRepository productRepository;
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
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .updateDate(new Date(System.currentTimeMillis()))
                            .status(true)
                    .build());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public Page<StockReplenishmentItemDTO> list(String user, Long orderId, String productSku, String sort, String sortColumn, Integer pageNumber, Integer pageSize) {
        Page<StockReplenishmentItem> pageStockReplenishmentItem;
        Long clientId;
        Long productId;

        if(productSku != null){
            productId = productRepository.findBySku(productSku.toUpperCase()).getId();
        }else {
            productId = null;
        }

        try {
            clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
            pageStockReplenishmentItem = stockReplenishmentItemRepositoryCustom.searchForStockReplenishmentItem(clientId,orderId,productId,sort,sortColumn,pageNumber,pageSize,true);
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.ResultsFound);
        }

        if(pageStockReplenishmentItem.isEmpty()){
            return new PageImpl<>(Collections.emptyList());
        }

        List<StockReplenishmentItemDTO> stockReplenishmentItemDTOS = pageStockReplenishmentItem.getContent().stream().map(stockReplenishmentItem -> StockReplenishmentItemDTO.builder()
                .productSku(stockReplenishmentItem.getProduct().getSku())
                .orderId(stockReplenishmentItem.getOrderId())
                .quantity(stockReplenishmentItem.getQuantity())
                .registrationDate(stockReplenishmentItem.getRegistrationDate())
                .updateDate(stockReplenishmentItem.getUpdateDate())
                .build()).toList();

        return new PageImpl<>(stockReplenishmentItemDTOS,pageStockReplenishmentItem.getPageable(),pageStockReplenishmentItem.getTotalElements());
    }

    @Override
    public List<StockReplenishmentItemDTO> listStockReplenishmentItem(String user) throws BadRequestExceptions, InternalErrorExceptions {
        List<StockReplenishmentItem> stockReplenishmentItems;
        Long clientId;
        try {
            clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
            stockReplenishmentItems = stockReplenishmentItemRepository.findAllByClientIdAndStatusTrue(clientId);
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if(stockReplenishmentItems.isEmpty()){
            return Collections.emptyList();
        }
        return stockReplenishmentItems.stream().map(stockReplenishmentItem -> StockReplenishmentItemDTO.builder()
                .productSku(stockReplenishmentItem.getProduct().getSku())
                .orderId(stockReplenishmentItem.getOrderId())
                .quantity(stockReplenishmentItem.getQuantity())
                .registrationDate(stockReplenishmentItem.getRegistrationDate())
                .updateDate(stockReplenishmentItem.getUpdateDate())
                .build()).toList();
    }

    @Override
    public List<StockReplenishmentItemDTO> listStockReplenishmentItemFalse(String user) throws BadRequestExceptions, InternalErrorExceptions {
        List<StockReplenishmentItem> stockReplenishmentItems;
        Long clientId;
        try {
            clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
            stockReplenishmentItems = stockReplenishmentItemRepository.findAllByClientIdAndStatusFalse(clientId);
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if(stockReplenishmentItems.isEmpty()){
            return Collections.emptyList();
        }
        return stockReplenishmentItems.stream().map(stockReplenishmentItem -> StockReplenishmentItemDTO.builder()
                .productSku(stockReplenishmentItem.getProduct().getSku())
                .orderId(stockReplenishmentItem.getOrderId())
                .quantity(stockReplenishmentItem.getQuantity())
                .registrationDate(stockReplenishmentItem.getRegistrationDate())
                .updateDate(stockReplenishmentItem.getUpdateDate())
                .build()).toList();
    }
}
