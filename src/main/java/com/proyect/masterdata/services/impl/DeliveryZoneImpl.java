package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.DeliveryZone;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.DeliveryZoneDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.DeliveryZoneRepository;
import com.proyect.masterdata.repository.DeliveryZoneRepositoryCustom;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IDeliveryZone;
import com.proyect.masterdata.utils.Constants;
import lombok.AllArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.OffsetDateTime;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@AllArgsConstructor
@Log4j2
public class DeliveryZoneImpl implements IDeliveryZone {
    private final UserRepository userRepository;
    private final DeliveryZoneRepository deliveryZoneRepository;
    private final IAudit iAudit;
    private final DeliveryZoneRepositoryCustom deliveryZoneRepositoryCustom;
    @Override
    public CompletableFuture<ResponseSuccess> save(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            DeliveryZone deliveryZone;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }else{
                deliveryZone = deliveryZoneRepository.findByNameAndClientId(name.toUpperCase(),user.getClientId());
            }
            if (deliveryZone != null) {
                throw new BadRequestExceptions(Constants.ErrorDeliveryZoneExists.toUpperCase());
            }

            try {
                DeliveryZone newDeliveryZone = deliveryZoneRepository.save(DeliveryZone.builder()
                        .name(name.toUpperCase())
                        .registrationDate(OffsetDateTime.now())
                        .status(true)
                        .user(user)
                        .userId(user.getId())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .build());
                iAudit.save("ADD_COLOR","COLOR "+newDeliveryZone.getName()+" CREADO.",newDeliveryZone.getName(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    @Transactional
    public CompletableFuture<ResponseDelete> delete(String name, String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            DeliveryZone deliveryZone;

            try {
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e);
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }else{
                deliveryZone = deliveryZoneRepository.findByNameAndClientIdAndStatusTrue(name.toUpperCase(),user.getClientId());
            }
            if (deliveryZone == null) {
                throw new BadRequestExceptions(Constants.ErrorDeliveryZone.toUpperCase());
            }

            try {
                deliveryZone.setStatus(false);
                deliveryZone.setUpdateDate(OffsetDateTime.now());
                deliveryZone.setUser(user);
                deliveryZone.setUserId(user.getId());
                deliveryZoneRepository.save(deliveryZone);
                iAudit.save("DELETE_COLOR","COLOR "+deliveryZone.getName()+" DESACTIVADO.", deliveryZone.getName(), user.getUsername());
                return ResponseDelete.builder()
                        .code(200)
                        .message(Constants.delete)
                        .build();
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> activate(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            DeliveryZone deliveryZone;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e);
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }else{
                deliveryZone = deliveryZoneRepository.findByNameAndClientIdAndStatusFalse(name.toUpperCase(),user.getClientId());
            }
            if (deliveryZone == null) {
                throw new BadRequestExceptions(Constants.ErrorDeliveryZone.toUpperCase());
            }

            try {
                deliveryZone.setStatus(true);
                deliveryZone.setUpdateDate(OffsetDateTime.now());
                deliveryZone.setUser(user);
                deliveryZone.setUserId(user.getId());
                deliveryZoneRepository.save(deliveryZone);
                iAudit.save("ACTIVATE_COLOR","COLOR "+deliveryZone.getName()+" ACTIVADO.",deliveryZone.getName(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.update)
                        .build();
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<List<DeliveryZoneDTO>> listDeliveryZone(String username) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<DeliveryZone> deliveryZones;
            User user;
            try {
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                deliveryZones = deliveryZoneRepository.findAllByStatusTrueAndClientId(user.getClientId());
            }
            if (deliveryZones.isEmpty()) {
                return Collections.emptyList();
            }
            return deliveryZones.stream().map(deliveryZone -> DeliveryZoneDTO.builder()
                    .id(deliveryZone.getId())
                    .updateDate(deliveryZone.getUpdateDate())
                    .name(deliveryZone.getName())
                    .registrationDate(deliveryZone.getRegistrationDate())
                    .user(deliveryZone.getUser().getUsername())
                    .status(deliveryZone.getStatus())
                    .build()).toList();
        });
    }

    @Override
    public CompletableFuture<Page<DeliveryZoneDTO>> list(
            String name,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<DeliveryZone> deliveryZonePage;
            try {
                deliveryZonePage = deliveryZoneRepositoryCustom.searchForDeliveryZone(
                        name,
                        registrationStartDate,
                        registrationEndDate,
                        updateStartDate,
                        updateEndDate,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        status);
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if (deliveryZonePage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }
            List<DeliveryZoneDTO> deliveryZoneDTOS = deliveryZonePage.getContent().stream().map(deliveryZone -> DeliveryZoneDTO.builder()
                    .id(deliveryZone.getId())
                    .updateDate(deliveryZone.getUpdateDate())
                    .name(deliveryZone.getName())
                    .registrationDate(deliveryZone.getRegistrationDate())
                    .user(deliveryZone.getUser().getUsername())
                    .status(deliveryZone.getStatus())
                    .build()).toList();
            return new PageImpl<>(deliveryZoneDTOS,
                    deliveryZonePage.getPageable(), deliveryZonePage.getTotalElements());
        });
    }
    @Override
    public CompletableFuture<List<DeliveryZoneDTO>> listFilter(String username) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<DeliveryZone> deliveryZones;
            User user;
            try {
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                deliveryZones = deliveryZoneRepository.findAllByClientId(user.getClientId());
            }
            if (deliveryZones.isEmpty()) {
                return Collections.emptyList();
            }
            return deliveryZones.stream().map(deliveryZone -> DeliveryZoneDTO.builder()
                    .id(deliveryZone.getId())
                    .updateDate(deliveryZone.getUpdateDate())
                    .name(deliveryZone.getName())
                    .registrationDate(deliveryZone.getRegistrationDate())
                    .user(deliveryZone.getUser().getUsername())
                    .status(deliveryZone.getStatus())
                    .build()).toList();
        });
    }
}
