package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.DeliveryManifest;
import com.proyect.masterdata.domain.DeliveryManifestStatus;
import com.proyect.masterdata.domain.DeliveryStatus;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.DeliveryManifestStatusDTO;
import com.proyect.masterdata.dto.DeliveryStatusDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.DeliveryManifestStatusRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IDeliveryManifestStatus;
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
public class DeliveryManifestStatusImpl implements IDeliveryManifestStatus {
    UserRepository userRepository;
    DeliveryManifestStatusRepository deliveryManifestStatusRepository;
    IAudit iAudit;
    @Override
    public CompletableFuture<ResponseSuccess> save(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            DeliveryManifestStatus deliveryManifestStatus;
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                deliveryManifestStatus = deliveryManifestStatusRepository.findByName(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(deliveryManifestStatus==null){
                throw new BadRequestExceptions(Constants.ErrorDeliveryManifestStatusExists);
            }
            try{
                DeliveryManifestStatus newDeliveryManifestStatus = deliveryManifestStatusRepository.save(DeliveryManifestStatus.builder()
                        .name(name.toUpperCase())
                        .registrationDate(OffsetDateTime.now())
                        .updateDate(OffsetDateTime.now())
                        .status(true)
                        .build());
                iAudit.save("ADD_DELIVERY_MANIFEST_STATUS","ESTADO DE GUIA "+newDeliveryManifestStatus.getName()+" CREADO.",newDeliveryManifestStatus.getName(),user.getUsername());
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
            DeliveryManifestStatus deliveryManifestStatus;
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                deliveryManifestStatus = deliveryManifestStatusRepository.findByName(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(deliveryManifestStatus==null){
                throw new BadRequestExceptions(Constants.ErrorDeliveryStatus);
            }
            try{
                deliveryManifestStatus.setStatus(false);
                deliveryManifestStatus.setUpdateDate(OffsetDateTime.now());
                deliveryManifestStatus.setUser(user);
                deliveryManifestStatus.setUserId(user.getId());
                deliveryManifestStatusRepository.save(deliveryManifestStatus);
                iAudit.save("DELETE_DELIVERY_MANIFEST_STATUS","ESTADO DE ENTREGA "+deliveryManifestStatus.getName()+" ELIMINADO.",deliveryManifestStatus.getName(),user.getUsername());
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
            DeliveryManifestStatus deliveryManifestStatus;
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                deliveryManifestStatus = deliveryManifestStatusRepository.findByName(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(deliveryManifestStatus==null){
                throw new BadRequestExceptions(Constants.ErrorDeliveryStatus);
            }
            try{
                deliveryManifestStatus.setStatus(true);
                deliveryManifestStatus.setUpdateDate(OffsetDateTime.now());
                deliveryManifestStatus.setUser(user);
                deliveryManifestStatus.setUserId(user.getId());
                deliveryManifestStatusRepository.save(deliveryManifestStatus);
                iAudit.save("ACTIVATE_DELIVERY_MANIFEST_STATUS","ESTADO DE GUIA "+deliveryManifestStatus.getName()+" ACTIVADO.",deliveryManifestStatus.getName(),user.getUsername());
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
    public CompletableFuture<List<DeliveryManifestStatusDTO>> listDeliveryManifestStatus() throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<DeliveryManifestStatus> deliveryManifestStatuses;
            try {
                deliveryManifestStatuses = deliveryManifestStatusRepository.findAllByStatusTrue();
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if (deliveryManifestStatuses.isEmpty()) {
                return Collections.emptyList();
            }
            return deliveryManifestStatuses.stream().map(deliveryStatus -> DeliveryManifestStatusDTO.builder()
                    .name(deliveryStatus.getName())
                    .registrationDate(deliveryStatus.getRegistrationDate())
                    .updateDate(deliveryStatus.getUpdateDate())
                    .build()).toList();
        });
    }
}
