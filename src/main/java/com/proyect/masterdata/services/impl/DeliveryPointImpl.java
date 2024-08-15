package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.CancellationReason;
import com.proyect.masterdata.domain.DeliveryPoint;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.DeliveryPointRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IDeliveryPoint;
import com.proyect.masterdata.utils.Constants;
import lombok.AllArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@AllArgsConstructor
@Log4j2
public class DeliveryPointImpl implements IDeliveryPoint {
    private final UserRepository userRepository;
    private final DeliveryPointRepository deliveryPointRepository;
    private final IAudit iAudit;
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
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .updateDate(new Date(System.currentTimeMillis()))
                            .tokenUser(user.getUsername())
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
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .updateDate(new Date(System.currentTimeMillis()))
                        .tokenUser(user.getUsername())
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
}
