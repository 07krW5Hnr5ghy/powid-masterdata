package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.DeliveryPoint;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.DeliveryPointDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.DeliveryPointRepository;
import com.proyect.masterdata.repository.DeliveryPointRepositoryCustom;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IDeliveryPoint;
import com.proyect.masterdata.utils.Constants;
import lombok.AllArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.time.OffsetDateTime;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@AllArgsConstructor
@Log4j2
public class DeliveryPointImpl implements IDeliveryPoint {
    private final UserRepository userRepository;
    private final DeliveryPointRepository deliveryPointRepository;
    private final IAudit iAudit;
    private final DeliveryPointRepositoryCustom deliveryPointRepositoryCustom;
    @Override
    public ResponseSuccess save(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        User user;
        DeliveryPoint deliveryPoint;
        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            deliveryPoint = deliveryPointRepository.findByName(name.toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if(user==null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }
        if(deliveryPoint!=null){
            throw new BadRequestExceptions(Constants.ErrorDeliveryPointExist);
        }
        try {
            DeliveryPoint newDeliveryPoint = deliveryPointRepository.save(DeliveryPoint.builder()
                            .name(name.toUpperCase())
                            .status(true)
                            .registrationDate(OffsetDateTime.now())
                            .updateDate(OffsetDateTime.now())
                            .user(user)
                            .userId(user.getId())
                    .build());
            iAudit.save("ADD_DELIVERY_POINT","PUNTO DE ENTREGA "+newDeliveryPoint.getName()+" CREADO.",newDeliveryPoint.getName(),user.getUsername());
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
    public CompletableFuture<ResponseSuccess> saveAsync(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            DeliveryPoint deliveryPoint;
            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                deliveryPoint = deliveryPointRepository.findByName(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(deliveryPoint!=null){
                throw new BadRequestExceptions(Constants.ErrorDeliveryPointExist);
            }
            try {
                DeliveryPoint newDeliveryPoint = deliveryPointRepository.save(DeliveryPoint.builder()
                        .name(name.toUpperCase())
                        .status(true)
                        .registrationDate(OffsetDateTime.now())
                        .updateDate(OffsetDateTime.now())
                        .user(user)
                                .userId(user.getId())
                        .build());
                iAudit.save("ADD_DELIVERY_POINT","PUNTO DE ENTREGA "+newDeliveryPoint.getName()+" CREADO.",newDeliveryPoint.getName(),user.getUsername());
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
            DeliveryPoint deliveryPoint;
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                deliveryPoint = deliveryPointRepository.findByNameAndStatusTrue(tokenUser.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(deliveryPoint==null){
                throw new BadRequestExceptions(Constants.ErrorDeliveryPoint);
            }
            try{
                deliveryPoint.setStatus(false);
                deliveryPoint.setUser(user);
                deliveryPoint.setUserId(user.getId());
                deliveryPoint.setUpdateDate(OffsetDateTime.now());
                deliveryPointRepository.save(deliveryPoint);
                iAudit.save("DELETE_DELIVERY_POINT","PUNTO DE ENTREGA "+deliveryPoint.getName()+" ELIMINADO.",deliveryPoint.getName(),user.getUsername());
                return ResponseDelete.builder()
                        .message(Constants.delete)
                        .code(200)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.ErrorDeliveryPoint);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> activate(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            DeliveryPoint deliveryPoint;
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                deliveryPoint = deliveryPointRepository.findByNameAndStatusFalse(tokenUser.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(deliveryPoint==null){
                throw new BadRequestExceptions(Constants.ErrorDeliveryPoint);
            }
            try{
                deliveryPoint.setStatus(true);
                deliveryPoint.setUser(user);
                deliveryPoint.setUserId(user.getId());
                deliveryPoint.setUpdateDate(OffsetDateTime.now());
                deliveryPointRepository.save(deliveryPoint);
                iAudit.save("ACTIVATE_DELIVERY_POINT","PUNTO DE ENTREGA "+deliveryPoint.getName()+" ACTIVADO.",deliveryPoint.getName(),user.getUsername());
                return ResponseSuccess.builder()
                        .message(Constants.register)
                        .code(200)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.ErrorDeliveryPoint);
            }
        });
    }

    @Override
    public CompletableFuture<List<String>> listDeliveryPoints() throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<DeliveryPoint> deliveryPointList;
            try {
                deliveryPointList = deliveryPointRepository.findAllByStatusTrue();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(deliveryPointList.isEmpty()){
                return Collections.emptyList();
            }
            return deliveryPointList.stream().map(DeliveryPoint::getName).toList();
        });
    }

    @Override
    public CompletableFuture<List<String>> listFilter() throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<DeliveryPoint> deliveryPointList;
            try {
                deliveryPointList = deliveryPointRepository.findAll();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(deliveryPointList.isEmpty()){
                return Collections.emptyList();
            }
            return deliveryPointList.stream().map(DeliveryPoint::getName).toList();
        });
    }

    @Override
    public CompletableFuture<Page<DeliveryPointDTO>> list(String name, OffsetDateTime registrationStartDate, OffsetDateTime registrationEndDate, OffsetDateTime updateStartDate, OffsetDateTime updateEndDate, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<DeliveryPoint> deliveryPointPage;
            try{
                deliveryPointPage = deliveryPointRepositoryCustom.searchForDeliveryPoint(
                        name,
                        registrationStartDate,
                        registrationEndDate,
                        updateStartDate,
                        updateEndDate,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        true
                );
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if(deliveryPointPage.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }
            List<DeliveryPointDTO> deliveryPointDTOS = deliveryPointPage.getContent().stream().map(deliveryPoint -> DeliveryPointDTO.builder()
                    .status(deliveryPoint.getStatus())
                    .id(deliveryPoint.getId())
                    .user(deliveryPoint.getUser().getUsername())
                    .name(deliveryPoint.getName())
                    .registrationDate(deliveryPoint.getRegistrationDate())
                    .updateDate(deliveryPoint.getUpdateDate())
                    .build()).toList();
            return new PageImpl<>(deliveryPointDTOS,deliveryPointPage.getPageable(),deliveryPointPage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<Page<DeliveryPointDTO>> listFalse(String name, OffsetDateTime registrationStartDate, OffsetDateTime registrationEndDate, OffsetDateTime updateStartDate, OffsetDateTime updateEndDate, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<DeliveryPoint> deliveryPointPage;
            try{
                deliveryPointPage = deliveryPointRepositoryCustom.searchForDeliveryPoint(
                        name,
                        registrationStartDate,
                        registrationEndDate,
                        updateStartDate,
                        updateEndDate,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        false
                );
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if(deliveryPointPage.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }
            List<DeliveryPointDTO> deliveryPointDTOS = deliveryPointPage.getContent().stream().map(deliveryPoint -> DeliveryPointDTO.builder()
                    .status(deliveryPoint.getStatus())
                    .id(deliveryPoint.getId())
                    .user(deliveryPoint.getUser().getUsername())
                    .name(deliveryPoint.getName())
                    .registrationDate(deliveryPoint.getRegistrationDate())
                    .updateDate(deliveryPoint.getUpdateDate())
                    .build()).toList();
            return new PageImpl<>(deliveryPointDTOS,deliveryPointPage.getPageable(),deliveryPointPage.getTotalElements());
        });
    }
}
