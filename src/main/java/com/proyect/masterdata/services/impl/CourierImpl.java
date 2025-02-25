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
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.ICourier;
import com.proyect.masterdata.services.ICourierPicture;
import com.proyect.masterdata.services.IOrderLog;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.time.OffsetDateTime;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

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
    private final ICourierPicture iCourierPicture;
    private final IAudit iAudit;
    private final IOrderLog iOrderLog;
    @Override
    public CompletableFuture<ResponseSuccess> save(RequestCourier requestCourier, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
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
                Courier newCourier = courierRepository.save(Courier.builder()
                        .name(requestCourier.getCourier().toUpperCase())
                        .phone(requestCourier.getPhone())
                        .address(requestCourier.getAddress())
                        .plate(requestCourier.getPlate())
                        .registrationDate(OffsetDateTime.now())
                        .updateDate(OffsetDateTime.now())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .status(true)
                                .user(user)
                                .userId(user.getId())
                        .build());
                iAudit.save("ADD_COURIER","COURIER "+newCourier.getName()+" CREADO.",newCourier.getName(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseDelete> delete(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
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
                courier.setUpdateDate(OffsetDateTime.now());
                courierRepository.save(courier);
                iAudit.save("DELETE_COURIER","COURIER "+courier.getName()+" DESACTIVADO.",courier.getName(),user.getUsername());
                return ResponseDelete.builder()
                        .code(200)
                        .message(Constants.delete)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> activate(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Courier courier;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                courier = courierRepository.findByNameAndStatusFalse(name.toUpperCase());
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
                courier.setStatus(true);
                courier.setUpdateDate(OffsetDateTime.now());
                courierRepository.save(courier);
                iAudit.save("ACTIVATE_COURIER","COURIER "+courier.getName()+" ACTIVADO.",courier.getName(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.update)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<Page<CourierDTO>> list(
            String user,
            List<String> names,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<Courier> pageCourier;
            UUID clientId;
            List<String> namesUppercase;
            if(names != null && !names.isEmpty()){
                namesUppercase = names.stream().map(String::toUpperCase).toList();
            }else{
                namesUppercase = new ArrayList<>();
            }
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClient().getId();
                pageCourier = courierRepositoryCustom.searchForCourier(
                        clientId,
                        namesUppercase,
                        registrationStartDate,
                        registrationEndDate,
                        updateStartDate,
                        updateEndDate,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        true);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if(pageCourier.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }

            List<CourierDTO> courierDTOS = pageCourier.getContent().stream().map(courier -> CourierDTO.builder()
                    .name(courier.getName())
                    .phone(courier.getPhone())
                    .address(courier.getAddress())
                    .plate(courier.getPlate())
                    .registrationDate(courier.getRegistrationDate())
                    .updateDate(courier.getUpdateDate())
                    .build()).toList();

            return new PageImpl<>(courierDTOS,pageCourier.getPageable(),pageCourier.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<Page<CourierDTO>> listFalse(
            String user,
            List<String> names,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<Courier> pageCourier;
            UUID clientId;

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClient().getId();
                pageCourier = courierRepositoryCustom.searchForCourier(
                        clientId,
                        names,
                        registrationStartDate,
                        registrationEndDate,
                        updateStartDate,
                        updateEndDate,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        false);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if(pageCourier.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }

            List<CourierDTO> courierDTOS = pageCourier.getContent().stream().map(courier -> CourierDTO.builder()
                    .name(courier.getName())
                    .phone(courier.getPhone())
                    .address(courier.getAddress())
                    .plate(courier.getPlate())
                    .registrationDate(courier.getRegistrationDate())
                    .updateDate(courier.getUpdateDate())
                    .build()).toList();

            return new PageImpl<>(courierDTOS,pageCourier.getPageable(),pageCourier.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> updateOrder(UUID orderId, RequestCourierOrder requestCourierOrder, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(() -> {
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
                throw new BadRequestExceptions(Constants.ErrorOrderState);
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

                ordering.setUpdateDate(OffsetDateTime.now());
                Ordering updatedOrder;
                CompletableFuture<List<String>> deliveryPictures = iCourierPicture.uploadPicture(requestCourierOrder.getOrderPictures(),ordering.getId(),user.getUsername());
                if(!ordering.getDeliveryFlag() && !deliveryPictures.get().isEmpty()){
                    ordering.setDeliveryFlag(true);
                    updatedOrder = orderingRepository.save(ordering);
                }
                updatedOrder = orderingRepository.save(ordering);
                iOrderLog.save(updatedOrder.getUser(),updatedOrder);
                iAudit.save("UPDATE_COURIER_ORDER","PEDIDO "+ordering.getId()+" EDITADO POR COURIER.",ordering.getId().toString(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            }catch (RuntimeException | InterruptedException | ExecutionException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<List<CourierDTO>> listCouriers(String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            UUID clientId;
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
                    .name(courier.getName())
                    .phone(courier.getPhone())
                    .address(courier.getAddress())
                    .plate(courier.getPlate())
                    .registrationDate(courier.getRegistrationDate())
                    .updateDate(courier.getUpdateDate())
                    .build()).toList();
        });
    }

    @Override
    public CompletableFuture<List<CourierDTO>> listCouriersFalse(String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            UUID clientId;
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
                    .name(courier.getName())
                    .phone(courier.getPhone())
                    .address(courier.getAddress())
                    .plate(courier.getPlate())
                    .registrationDate(courier.getRegistrationDate())
                    .updateDate(courier.getUpdateDate())
                    .build()).toList();
        });
    }

    @Override
    public CompletableFuture<List<CourierDTO>> listFilters(String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            UUID clientId;
            List<Courier> couriers;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClient().getId();
                couriers = courierRepository.findAllByClientId(clientId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(couriers.isEmpty()){
                return Collections.emptyList();
            }
            List<CourierDTO> courierDTOS = new ArrayList<>(couriers.stream().map(courier -> CourierDTO.builder()
                    .name(courier.getName())
                    .phone(courier.getPhone())
                    .address(courier.getAddress())
                    .plate(courier.getPlate())
                    .registrationDate(courier.getRegistrationDate())
                    .updateDate(courier.getUpdateDate())
                    .build()).toList());
            Courier defaultNoCourier = courierRepository.findByNameAndStatusTrue("SIN COURIER");
            CourierDTO dtoNoCourier = CourierDTO.builder()
                    .name(defaultNoCourier.getName())
                    .phone(defaultNoCourier.getPhone())
                    .address(defaultNoCourier.getAddress())
                    .plate(defaultNoCourier.getPlate())
                    .registrationDate(defaultNoCourier.getRegistrationDate())
                    .updateDate(defaultNoCourier.getUpdateDate())
                    .build();
            courierDTOS.add(dtoNoCourier);
            return courierDTOS;
        });
    }
}
