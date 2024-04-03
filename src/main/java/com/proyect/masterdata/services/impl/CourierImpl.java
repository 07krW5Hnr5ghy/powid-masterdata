package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.CourierDTO;
import com.proyect.masterdata.dto.request.RequestCourier;
import com.proyect.masterdata.dto.request.RequestCourierOrder;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.ICourier;
import com.proyect.masterdata.services.ICourierPicture;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Objects;

@Service
@RequiredArgsConstructor
@Log4j2
public class CourierImpl implements ICourier {
    private final UserRepository userRepository;
    private final CourierRepository courierRepository;
    private final CourierRepositoryCustom courierRepositoryCustom;
    private final OrderingRepository orderingRepository;
    private final OrderStateRepository orderStateRepository;
    private final OrderPaymentMethodRepository orderPaymentMethodRepository;
    private final SaleRepository saleRepository;
    private final ICourierPicture iCourierPicture;
    @Override
    public ResponseSuccess save(RequestCourier requestCourier, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        Courier courier;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            courier = courierRepository.findByName(requestCourier.getCourier().toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(courier != null){
            throw new BadRequestExceptions(Constants.ErrorCourierExists);
        }

        try {
            courierRepository.save(Courier.builder()
                            .name(requestCourier.getCourier().toUpperCase())
                            .phoneNumber(requestCourier.getPhoneNumber())
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .updateDate(new Date(System.currentTimeMillis()))
                            .client(user.getClient())
                            .clientId(user.getClientId())
                            .status(true)
                            .tokenUser(user.getUsername())
                    .build());
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
    public ResponseDelete delete(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        Courier courier;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            courier = courierRepository.findByNameAndStatusTrue(name.toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(courier == null){
            throw new BadRequestExceptions(Constants.ErrorCourier);
        }

        try {
            courier.setStatus(false);
            courier.setUpdateDate(new Date(System.currentTimeMillis()));
            courierRepository.save(courier);
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

    }

    @Override
    public Page<CourierDTO> list(String name, String user, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        Page<Courier> pageCourier;
        Long clientId;
        try {
            clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClient().getId();
            pageCourier = courierRepositoryCustom.searchForCourier(name,clientId,sort,sortColumn,pageNumber,pageSize,true);
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.ResultsFound);
        }

        if(pageCourier.isEmpty()){
            return new PageImpl<>(Collections.emptyList());
        }

        List<CourierDTO> courierDTOS = pageCourier.getContent().stream().map(courier -> CourierDTO.builder()
                .courier(courier.getName())
                .phoneNumber(courier.getPhoneNumber())
                .build()).toList();

        return new PageImpl<>(courierDTOS,pageCourier.getPageable(),pageCourier.getTotalElements());
    }

    @Override
    public Page<CourierDTO> listFalse(String name, String user, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions {

        Page<Courier> pageCourier;
        Long clientId;

        try {
            clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClient().getId();
            pageCourier = courierRepositoryCustom.searchForCourier(name,clientId,sort,sortColumn,pageNumber,pageSize,false);
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.ResultsFound);
        }

        if(pageCourier.isEmpty()){
            return new PageImpl<>(Collections.emptyList());
        }

        List<CourierDTO> courierDTOS = pageCourier.getContent().stream().map(courier -> CourierDTO.builder()
                .courier(courier.getName())
                .phoneNumber(courier.getPhoneNumber())
                .build()).toList();

        return new PageImpl<>(courierDTOS,pageCourier.getPageable(),pageCourier.getTotalElements());
    }

    @Override
    public ResponseSuccess updateOrder(Long orderId, RequestCourierOrder requestCourierOrder, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        Ordering ordering;
        OrderState orderState;
        OrderPaymentMethod orderPaymentMethod;

        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            ordering = orderingRepository.findById(orderId).orElse(null);
            orderState = orderStateRepository.findByNameAndStatusTrue(requestCourierOrder.getOrderState().toUpperCase());
            orderPaymentMethod = orderPaymentMethodRepository.findByNameAndStatusTrue(requestCourierOrder.getPaymentMethod().toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(ordering == null){
            throw new BadRequestExceptions(Constants.ErrorOrdering);
        }

        if(orderState == null){
            throw new BadRequestExceptions(Constants.ErrorState);
        }

        try{

            if(("EN RUTA".equals(ordering.getOrderState().getName())) && !Objects.equals(orderState.getId(), ordering.getOrderStateId())){
                ordering.setOrderState(orderState);
                ordering.setOrderStateId(orderState.getId());
            }

            if(!Objects.equals(orderPaymentMethod.getId(), ordering.getPaymentMethodId())){
                ordering.setOrderPaymentMethod(orderPaymentMethod);
                ordering.setPaymentMethodId(orderPaymentMethod.getId());
            }

            ordering.setUpdateDate(new Date(System.currentTimeMillis()));

            iCourierPicture.uploadPicture(requestCourierOrder.getOrderPictures(),ordering.getId(),user.getUsername());
            orderingRepository.save(ordering);

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
    public List<CourierDTO> listCouriers(String user) throws BadRequestExceptions, InternalErrorExceptions {
        Long clientId;
        List<Courier> couriers;
        try {
            clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClient().getId();
            couriers = courierRepository.findAllByClientIdAndStatusTrue(clientId);
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if(couriers.isEmpty()){
            return Collections.emptyList();
        }
        return couriers.stream().map(courier -> CourierDTO.builder()
                .courier(courier.getName())
                .phoneNumber(courier.getPhoneNumber())
                .build()).toList();
    }

    @Override
    public List<CourierDTO> listCouriersFalse(String user) throws BadRequestExceptions, InternalErrorExceptions {
        Long clientId;
        List<Courier> couriers;
        try {
            clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClient().getId();
            couriers = courierRepository.findAllByClientIdAndStatusFalse(clientId);
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if(couriers.isEmpty()){
            return Collections.emptyList();
        }
        return couriers.stream().map(courier -> CourierDTO.builder()
                .courier(courier.getName())
                .phoneNumber(courier.getPhoneNumber())
                .build()).toList();
    }
}
