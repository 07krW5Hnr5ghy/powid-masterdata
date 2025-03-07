package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.CancelledOrderDTO;
import com.proyect.masterdata.dto.request.RequestCancelledOrder;
import com.proyect.masterdata.dto.request.RequestStockTransactionItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.ICancelledOrder;
import com.proyect.masterdata.services.IStockTransaction;
import com.proyect.masterdata.services.IUtil;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class CancelledOrderImpl implements ICancelledOrder {
    private final CancelledOrderRepository cancelledOrderRepository;
    private final UserRepository userRepository;
    private final OrderingRepository orderingRepository;
    private final CancellationReasonRepository cancellationReasonRepository;
    private final CancelledOrderRepositoryCustom cancelledOrderRepositoryCustom;
    private final OrderStateRepository orderStateRepository;
    private final IStockTransaction iStockTransaction;
    private final OrderItemRepository orderItemRepository;
    private final WarehouseRepository warehouseRepository;
    private final IAudit iAudit;
    private final IUtil iUtil;
    @Override
    public CompletableFuture<ResponseSuccess> save(RequestCancelledOrder requestCancelledOrder, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            CancelledOrder cancelledOrder;
            Ordering ordering;
            CancellationReason cancellationReason;
            OrderState orderState;
            Warehouse warehouseData;
            List<OrderItem> orderItemList;

            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                ordering = orderingRepository.findById(requestCancelledOrder.getOrderId()).orElse(null);
                cancellationReason = cancellationReasonRepository.findByNameAndStatusTrue(requestCancelledOrder.getCancellationReason().toUpperCase());
                warehouseData = warehouseRepository.findByNameAndStatusTrue(requestCancelledOrder.getWarehouse().toUpperCase());
                orderState = orderStateRepository.findByNameAndStatusTrue("CANCELADO");
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else {
                cancelledOrder = cancelledOrderRepository.findByOrderingId(requestCancelledOrder.getOrderId());
            }

            if (cancelledOrder != null){
                throw new BadRequestExceptions(Constants.ErrorCancelledOrderExists);
            }

            if(ordering == null){
                throw new BadRequestExceptions(Constants.ErrorOrdering);
            }

            if(warehouseData == null){
                throw new BadRequestExceptions(Constants.ErrorWarehouse);
            }

            if(cancellationReason == null){
                throw new BadRequestExceptions(Constants.ErrorCancellationReason);
            }

            try {
                cancelledOrderRepository.save(CancelledOrder.builder()
                        .ordering(ordering)
                        .orderingId(ordering.getId())
                        .cancellationReason(cancellationReason)
                        .cancellationReasonId(cancellationReason.getId())
                        .registrationDate(OffsetDateTime.now())
                        .updateDate(OffsetDateTime.now())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                                .user(user)
                                .userId(user.getId())
                        .build());
                ordering.setOrderState(orderState);
                ordering.setOrderStateId(orderState.getId());
                ordering.setCancellation(true);
                orderingRepository.save(ordering);
                iAudit.save("ADD_CANCELLED_ORDER","PEDIDO CANCELADO #" + ordering.getId() + ".",ordering.getId().toString(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            } catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<Page<CancelledOrderDTO>> list(
            UUID orderId, 
            String user,
            OffsetDateTime registrationStartDate, 
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort, 
            String sortColumn, 
            Integer pageNumber, 
            Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<CancelledOrder> pageCancelledOrder;
            UUID clientId;

            try{
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClient().getId();
                pageCancelledOrder = cancelledOrderRepositoryCustom.searchForCancelledOrder(
                        orderId,
                        clientId,
                        registrationStartDate,
                        registrationEndDate,
                        updateStartDate,
                        updateEndDate,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if(pageCancelledOrder.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }

            List<CancelledOrderDTO> cancelledOrderDTOS = pageCancelledOrder.getContent().stream().map(cancelledOrder -> CancelledOrderDTO.builder()
                    .id(cancelledOrder.getId())
                    .user(cancelledOrder.getUser().getUsername())
                    .orderId(cancelledOrder.getOrderingId())
                    .cancellationReason(cancelledOrder.getCancellationReason().getName())
                    .registrationDate(cancelledOrder.getRegistrationDate())
                    .updateDate(cancelledOrder.getUpdateDate())
                    .build()).toList();

            return new PageImpl<>(cancelledOrderDTOS,pageCancelledOrder.getPageable(),pageCancelledOrder.getTotalElements());
        });
    }
}
