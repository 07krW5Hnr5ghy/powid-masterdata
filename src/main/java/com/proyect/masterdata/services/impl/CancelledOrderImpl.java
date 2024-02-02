package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.CancelledOrderDTO;
import com.proyect.masterdata.dto.request.RequestCancelledOrder;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.ICancelledOrder;
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
public class CancelledOrderImpl implements ICancelledOrder {
    private final CancelledOrderRepository cancelledOrderRepository;
    private final UserRepository userRepository;
    private final OrderingRepository orderingRepository;
    private final CancellationReasonRepository cancellationReasonRepository;
    private final CancelledOrderRepositoryCustom cancelledOrderRepositoryCustom;
    private final OrderStateRepository orderStateRepository;
    @Override
    public ResponseSuccess save(RequestCancelledOrder requestCancelledOrder, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        CancelledOrder cancelledOrder;
        Ordering ordering;
        CancellationReason cancellationReason;
        OrderState orderState;

        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            ordering = orderingRepository.findById(requestCancelledOrder.getOrderId()).orElse(null);
            cancellationReason = cancellationReasonRepository.findByNameAndStatusTrue(requestCancelledOrder.getCancellationReason().toUpperCase());
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

        if(cancellationReason == null){
            throw new BadRequestExceptions(Constants.ErrorCancellationReason);
        }

        try {
            cancelledOrderRepository.save(CancelledOrder.builder()
                            .ordering(ordering)
                            .orderingId(ordering.getId())
                            .cancellationReason(cancellationReason)
                            .cancellationReasonId(cancellationReason.getId())
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .updateDate(new Date(System.currentTimeMillis()))
                            .client(user.getClient())
                            .clientId(user.getClientId())
                            .tokenUser(user.getUsername())
                    .build());
            ordering.setOrderState(orderState);
            ordering.setOrderStateId(orderState.getId());
            ordering.setCancellation(true);
            orderingRepository.save(ordering);
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public Page<CancelledOrderDTO> list(Long orderId, String user, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        Page<CancelledOrder> pageCancelledOrder;
        Long clientId;

        try{
            clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClient().getId();
            pageCancelledOrder = cancelledOrderRepositoryCustom.searchForCancelledOrder(orderId,clientId,sort,sortColumn,pageNumber,pageSize);
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.ResultsFound);
        }

        if(pageCancelledOrder.isEmpty()){
            return new PageImpl<>(Collections.emptyList());
        }

        List<CancelledOrderDTO> cancelledOrderDTOS = pageCancelledOrder.getContent().stream().map(cancelledOrder -> {
            return CancelledOrderDTO.builder()
                    .orderId(cancelledOrder.getOrderingId())
                    .cancellationReason(cancelledOrder.getCancellationReason().getName())
                    .registrationDate(cancelledOrder.getRegistrationDate())
                    .build();
        }).toList();

        return new PageImpl<>(cancelledOrderDTOS,pageCancelledOrder.getPageable(),pageCancelledOrder.getTotalElements());
    }
}
