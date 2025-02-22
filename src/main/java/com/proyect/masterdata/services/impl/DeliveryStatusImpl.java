package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.DeliveryStatus;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.DeliveryStatusDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.DeliveryStatusRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IDeliveryStatus;
import com.proyect.masterdata.utils.Constants;
import lombok.AllArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.time.OffsetDateTime;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@AllArgsConstructor
@Log4j2
public class DeliveryStatusImpl implements IDeliveryStatus {
    private final UserRepository userRepository;
    private final DeliveryStatusRepository deliveryStatusRepository;
    private final IAudit iAudit;
    @Override
    public CompletableFuture<ResponseSuccess> save(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            DeliveryStatus deliveryStatus;
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                deliveryStatus = deliveryStatusRepository.findByName(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(deliveryStatus==null){
                throw new BadRequestExceptions(Constants.ErrorDeliveryStatus);
            }
            try{
                DeliveryStatus newDeliveryStatus = deliveryStatusRepository.save(DeliveryStatus.builder()
                        .name(name.toUpperCase())
                        .registrationDate(OffsetDateTime.now())
                        .updateDate(OffsetDateTime.now())
                        .status(true)
                        .build());
                iAudit.save("ADD_DELIVERY_STATUS","ESTADO DE ENTREGA "+newDeliveryStatus.getName()+" CREADO.",newDeliveryStatus.getName(),user.getUsername());
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
    public CompletableFuture<ResponseDelete> delete(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            DeliveryStatus deliveryStatus;
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                deliveryStatus = deliveryStatusRepository.findByName(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(deliveryStatus==null){
                throw new BadRequestExceptions(Constants.ErrorDeliveryStatus);
            }
            try{
                deliveryStatus.setStatus(false);
                deliveryStatus.setUpdateDate(OffsetDateTime.now());
                deliveryStatus.setUser(user);
                deliveryStatus.setUserId(user.getId());
                iAudit.save("DELETE_DELIVERY_STATUS","ESTADO DE ENTREGA "+deliveryStatus.getName()+" ELIMINADO.",deliveryStatus.getName(),user.getUsername());
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
    public CompletableFuture<ResponseSuccess> activate(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            DeliveryStatus deliveryStatus;
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                deliveryStatus = deliveryStatusRepository.findByName(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(deliveryStatus==null){
                throw new BadRequestExceptions(Constants.ErrorDeliveryStatus);
            }
            try{
                deliveryStatus.setStatus(true);
                deliveryStatus.setUpdateDate(OffsetDateTime.now());
                deliveryStatus.setUser(user);
                deliveryStatus.setUserId(user.getId());
                iAudit.save("ACTIVATE_DELIVERY_STATUS","ESTADO DE ENTREGA "+deliveryStatus.getName()+" ACTIVADO.",deliveryStatus.getName(),user.getUsername());
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
    public CompletableFuture<List<DeliveryStatusDTO>> listDeliveryStatus() throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<DeliveryStatus> deliveryStatuses;
            try {
                deliveryStatuses = deliveryStatusRepository.findAllByStatusTrue();
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if (deliveryStatuses.isEmpty()) {
                return Collections.emptyList();
            }
            return deliveryStatuses.stream().map(deliveryStatus -> DeliveryStatusDTO.builder()
                    .name(deliveryStatus.getName())
                    .registrationDate(deliveryStatus.getRegistrationDate())
                    .updateDate(deliveryStatus.getUpdateDate())
                    .build()).toList();
        });
    }
}
