package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.StockReplenishmentDTO;
import com.proyect.masterdata.dto.request.RequestStockReplenishmentItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IStockReplenishment;
import com.proyect.masterdata.services.IStockReplenishmentItem;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class StockReplenishmentImpl implements IStockReplenishment {
    private final UserRepository userRepository;
    private final StockReplenishmentRepository stockReplenishmentRepository;
    private final OrderingRepository orderingRepository;
    private final OrderItemRepository orderItemRepository;
    private final ProductRepository productRepository;
    private final IStockReplenishmentItem iStockReplenishmentItem;
    private final StockReplenishmentRepositoryCustom stockReplenishmentRepositoryCustom;
    private final IAudit iAudit;
    @Override
    public ResponseSuccess save(Long orderId, List<RequestStockReplenishmentItem> requestStockReplenishmentItems, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        StockReplenishment stockReplenishment;
        Ordering ordering;
        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            stockReplenishment = stockReplenishmentRepository.findByOrderId(orderId);
            ordering = orderingRepository.findById(orderId).orElse(null);
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(stockReplenishment != null){
            throw new BadRequestExceptions(Constants.ErrorStockReplenishmentExists);
        }

        if(ordering == null){
            throw new BadRequestExceptions(Constants.ErrorOrdering);
        }

        if(!ordering.getOrderState().getName().equals("NO HAY STOCK")){
            throw new BadRequestExceptions(Constants.ErrorStockReplenishmentOrderState);
        }

        try{
            for (RequestStockReplenishmentItem requestStockReplenishmentItem : requestStockReplenishmentItems){
                if(requestStockReplenishmentItem.getQuantity() < 1){
                    throw new BadRequestExceptions(Constants.ErrorStockReplenishmentItemZero);
                }
                Product product = productRepository.findBySku(requestStockReplenishmentItem.getProductSku().toUpperCase());
                if(product == null){
                    throw new BadRequestExceptions(Constants.ErrorProduct);
                }
                OrderItem orderItem = orderItemRepository.findByOrderIdAndProductId(ordering.getId(),product.getId());
                if(requestStockReplenishmentItem.getQuantity() > orderItem.getQuantity()){
                    throw new BadRequestExceptions(Constants.ErrorStockReplenishmentQuantity);
                }
            }
            StockReplenishment newStockReplenishment = stockReplenishmentRepository.save(StockReplenishment.builder()
                            .ordering(ordering)
                            .orderId(ordering.getId())
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .updateDate(new Date(System.currentTimeMillis()))
                            .status(true)
                            .client(user.getClient())
                            .clientId(user.getClientId())
                            .tokenUser(user.getUsername())
                    .build());
            for (RequestStockReplenishmentItem requestStockReplenishmentItem : requestStockReplenishmentItems){
                Product product = productRepository.findBySku(requestStockReplenishmentItem.getProductSku().toUpperCase());
                OrderItem orderItem = orderItemRepository.findByOrderIdAndProductId(ordering.getId(),product.getId());
                iStockReplenishmentItem.save(orderItem,requestStockReplenishmentItem,user,newStockReplenishment);
            }
            iAudit.save("ADD_STOCK_REPLENISHMENT","RESTOCKAJE PARA PEDIDO "+newStockReplenishment.getOrderId()+" CREADO.",newStockReplenishment.getOrderId().toString(),user.getUsername());
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        }catch (RuntimeException e){
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public CompletableFuture<ResponseSuccess> saveAsync(Long orderId, List<RequestStockReplenishmentItem> requestStockReplenishmentItems, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            StockReplenishment stockReplenishment;
            Ordering ordering;
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                stockReplenishment = stockReplenishmentRepository.findByOrderId(orderId);
                ordering = orderingRepository.findById(orderId).orElse(null);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(stockReplenishment != null){
                throw new BadRequestExceptions(Constants.ErrorStockReplenishmentExists);
            }

            if(ordering == null){
                throw new BadRequestExceptions(Constants.ErrorOrdering);
            }

            if(!ordering.getOrderState().getName().equals("NO HAY STOCK")){
                throw new BadRequestExceptions(Constants.ErrorStockReplenishmentOrderState);
            }

            try{
                for (RequestStockReplenishmentItem requestStockReplenishmentItem : requestStockReplenishmentItems){
                    if(requestStockReplenishmentItem.getQuantity() < 1){
                        throw new BadRequestExceptions(Constants.ErrorStockReplenishmentItemZero);
                    }
                    Product product = productRepository.findBySku(requestStockReplenishmentItem.getProductSku().toUpperCase());
                    if(product == null){
                        throw new BadRequestExceptions(Constants.ErrorProduct);
                    }
                    OrderItem orderItem = orderItemRepository.findByOrderIdAndProductId(ordering.getId(),product.getId());
                    if(requestStockReplenishmentItem.getQuantity() > orderItem.getQuantity()){
                        throw new BadRequestExceptions(Constants.ErrorStockReplenishmentQuantity);
                    }
                }
                StockReplenishment newStockReplenishment = stockReplenishmentRepository.save(StockReplenishment.builder()
                        .ordering(ordering)
                        .orderId(ordering.getId())
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .updateDate(new Date(System.currentTimeMillis()))
                        .status(true)
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .tokenUser(user.getUsername())
                        .build());
                for (RequestStockReplenishmentItem requestStockReplenishmentItem : requestStockReplenishmentItems){
                    Product product = productRepository.findBySku(requestStockReplenishmentItem.getProductSku().toUpperCase());
                    OrderItem orderItem = orderItemRepository.findByOrderIdAndProductId(ordering.getId(),product.getId());
                    iStockReplenishmentItem.save(orderItem,requestStockReplenishmentItem,user,newStockReplenishment);
                }
                iAudit.save("ADD_STOCK_REPLENISHMENT","RESTOCKAJE PARA PEDIDO "+newStockReplenishment.getOrderId()+" CREADO.",newStockReplenishment.getOrderId().toString(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            }catch (RuntimeException e){
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<Page<StockReplenishmentDTO>> list(
            String user,
            List<Long> orders,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<StockReplenishment> pageStockReplenishment;
            Long clientId;
            List<Long> orderIds;

            if(orders != null && !orders.isEmpty()){
                orderIds = orders;
            }else {
                orderIds = new ArrayList<>();
            }

            try{
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                pageStockReplenishment = stockReplenishmentRepositoryCustom.searchForStockReplenishment(
                        clientId,
                        orderIds,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        true);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if(pageStockReplenishment.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }

            List<StockReplenishmentDTO> stockReplenishmentDTOS = pageStockReplenishment.getContent().stream().map(stockReplenishment -> StockReplenishmentDTO.builder()
                    .orderId(stockReplenishment.getOrderId())
                    .registrationDate(stockReplenishment.getRegistrationDate())
                    .updateDate(stockReplenishment.getUpdateDate())
                    .build()
            ).toList();
            return new PageImpl<>(stockReplenishmentDTOS,pageStockReplenishment.getPageable(),pageStockReplenishment.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<StockReplenishmentDTO>> listStockReplenishment(String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<StockReplenishment> stockReplenishments;
            Long clientId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                stockReplenishments = stockReplenishmentRepository.findAllByClientIdAndStatusTrue(clientId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(stockReplenishments.isEmpty()){
                return Collections.emptyList();
            }
            return stockReplenishments.stream().map(stockReplenishment -> StockReplenishmentDTO.builder()
                    .orderId(stockReplenishment.getOrderId())
                    .registrationDate(stockReplenishment.getRegistrationDate())
                    .updateDate(stockReplenishment.getUpdateDate())
                    .build()
            ).toList();
        });
    }

    @Override
    public CompletableFuture<List<StockReplenishmentDTO>> listStockReplenishmentFalse(String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<StockReplenishment> stockReplenishments;
            Long clientId;
            try {
                clientId = userRepository.findByUsernameAndStatusFalse(user.toUpperCase()).getClientId();
                stockReplenishments = stockReplenishmentRepository.findAllByClientIdAndStatusFalse(clientId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(stockReplenishments.isEmpty()){
                return Collections.emptyList();
            }
            return stockReplenishments.stream().map(stockReplenishment -> StockReplenishmentDTO.builder()
                    .orderId(stockReplenishment.getOrderId())
                    .registrationDate(stockReplenishment.getRegistrationDate())
                    .updateDate(stockReplenishment.getUpdateDate())
                    .build()
            ).toList();
        });
    }
}
