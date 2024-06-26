package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.ShipmentType;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.ShipmentTypeRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IShipmentType;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class ShipmentTypeImpl implements IShipmentType {

    private final UserRepository userRepository;
    private final ShipmentTypeRepository shipmentTypeRepository;
    private final IAudit iAudit;
    @Override
    public ResponseSuccess save(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        ShipmentType shipmentType;

        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            shipmentType = shipmentTypeRepository.findByName(name.toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(shipmentType != null){
            throw new BadRequestExceptions(Constants.ErrorShipmentTypeExists);
        }

        try{
            ShipmentType newShipmentType = shipmentTypeRepository.save(ShipmentType.builder()
                            .name(name.toUpperCase())
                            .status(true)
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .tokenUser(user.getUsername())
                    .build());
            iAudit.save("ADD_SHIPMENT_TYPE","ADD SHIPMENT TYPE "+newShipmentType.getName()+".",user.getUsername());
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
    public CompletableFuture<ResponseSuccess> saveAsync(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            ShipmentType shipmentType;

            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                shipmentType = shipmentTypeRepository.findByName(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(shipmentType != null){
                throw new BadRequestExceptions(Constants.ErrorShipmentTypeExists);
            }

            try{
                ShipmentType newShipmentType = shipmentTypeRepository.save(ShipmentType.builder()
                        .name(name.toUpperCase())
                        .status(true)
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .tokenUser(user.getUsername())
                        .build());
                iAudit.save("ADD_SHIPMENT_TYPE","ADD SHIPMENT TYPE "+newShipmentType.getName()+".",user.getUsername());
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
            ShipmentType shipmentType;

            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                shipmentType = shipmentTypeRepository.findByNameAndStatusTrue(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(shipmentType == null){
                throw new BadRequestExceptions(Constants.ErrorShipmentType);
            }

            try{
                shipmentType.setStatus(false);
                shipmentType.setTokenUser(user.getUsername());
                shipmentType.setUpdateDate(new Date(System.currentTimeMillis()));
                shipmentTypeRepository.save(shipmentType);
                iAudit.save("DELETE_SHIPMENT_TYPE","DELETE SHIPMENT TYPE "+shipmentType.getName()+".",user.getUsername());
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
            ShipmentType shipmentType;

            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                shipmentType = shipmentTypeRepository.findByNameAndStatusFalse(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(shipmentType == null){
                throw new BadRequestExceptions(Constants.ErrorShipmentType);
            }

            try{
                shipmentType.setStatus(true);
                shipmentType.setTokenUser(user.getUsername());
                shipmentType.setUpdateDate(new Date(System.currentTimeMillis()));
                shipmentTypeRepository.save(shipmentType);
                iAudit.save("ACTIVATE_SHIPMENT_TYPE","ACTIVATE SHIPMENT TYPE "+shipmentType.getName()+".",user.getUsername());
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
    public CompletableFuture<List<String>> list() throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<ShipmentType> shipmentTypeList;

            try{
                shipmentTypeList = shipmentTypeRepository.findAllByStatusTrue();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if(shipmentTypeList.isEmpty()){
                return Collections.emptyList();
            }

            return shipmentTypeList.stream().map(ShipmentType::getName).toList();
        });
    }

    @Override
    public CompletableFuture<List<String>> listFilter() throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<ShipmentType> shipmentTypeList;

            try{
                shipmentTypeList = shipmentTypeRepository.findAll();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if(shipmentTypeList.isEmpty()){
                return Collections.emptyList();
            }

            return shipmentTypeList.stream().map(ShipmentType::getName).toList();
        });
    }
}
