package com.proyect.masterdata.services.impl;

import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.repository.ClosingChannelRepositoryCustom;
import com.proyect.masterdata.services.IAudit;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import com.proyect.masterdata.domain.ClosingChannel;
import com.proyect.masterdata.dto.ClosingChannelDTO;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.mapper.ClosingChannelMapper;
import com.proyect.masterdata.repository.ClosingChannelRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IClosingChannel;
import com.proyect.masterdata.utils.Constants;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class ClosingChannelImpl implements IClosingChannel {
    private final ClosingChannelRepository closingChannelRepository;
    private final UserRepository userRepository;
    private final ClosingChannelMapper closingChannelMapper;
    private final ClosingChannelRepositoryCustom closingChannelRepositoryCustom;
    private final IAudit iAudit;

    @Override
    public ResponseSuccess save(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        ClosingChannel closingChannel;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            closingChannel = closingChannelRepository.findByNameAndStatusTrue(name.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.InternalErrorExceptions);
        }

        if (user==null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (closingChannel!=null) {
            throw new BadRequestExceptions(Constants.ErrorClosingChannelExists);
        }

        try {
            ClosingChannel newClosingChannel = closingChannelRepository.save(ClosingChannel.builder()
                    .name(name.toUpperCase())
                    .status(true)
                    .registrationDate(OffsetDateTime.now())
                    .updateDate(OffsetDateTime.now())
                            .user(user)
                            .userId(user.getId())
                    .build());
            iAudit.save("ADD_CLOSING_CHANNEL","CANAL DE CIERRE "+newClosingChannel.getName()+" CREADO.",newClosingChannel.getName(),user.getUsername());
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public CompletableFuture<ResponseSuccess> saveAsync(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            ClosingChannel closingChannel;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                closingChannel = closingChannelRepository.findByNameAndStatusTrue(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }

            if (user==null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if (closingChannel!=null) {
                throw new BadRequestExceptions(Constants.ErrorClosingChannelExists);
            }

            try {
                ClosingChannel newClosingChannel = closingChannelRepository.save(ClosingChannel.builder()
                        .name(name.toUpperCase())
                        .status(true)
                        .registrationDate(OffsetDateTime.now())
                        .updateDate(OffsetDateTime.now())
                        .user(user)
                        .userId(user.getId())
                        .build());
                iAudit.save("ADD_CLOSING_CHANNEL","CANAL DE CIERRE "+newClosingChannel.getName()+" CREADO.",newClosingChannel.getName(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<Page<ClosingChannelDTO>> listClosingChannel(
            String name,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<ClosingChannel> closingChannelPage;
            try {
                closingChannelPage = closingChannelRepositoryCustom.searchForClosingChannel(
                        name,
                        registrationStartDate,
                        registrationEndDate,
                        updateStartDate,
                        updateStartDate,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        true);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if(closingChannelPage.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }

            List<ClosingChannelDTO> closingChannelDTOS = closingChannelMapper.listClosingChannelToListClosingChannelDTO(closingChannelPage.getContent());
            return new PageImpl<>(closingChannelDTOS,closingChannelPage.getPageable(),closingChannelPage.getTotalElements());
        });

    }

    @Override
    public CompletableFuture<Page<ClosingChannelDTO>> listFalse(String name, OffsetDateTime registrationStartDate, OffsetDateTime registrationEndDate, OffsetDateTime updateStartDate, OffsetDateTime updateEndDate, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<ClosingChannel> closingChannelPage;
            try {
                closingChannelPage = closingChannelRepositoryCustom.searchForClosingChannel(
                        name,
                        registrationStartDate,
                        registrationEndDate,
                        updateStartDate,
                        updateStartDate,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        false);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if(closingChannelPage.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }

            List<ClosingChannelDTO> closingChannelDTOS = closingChannelMapper.listClosingChannelToListClosingChannelDTO(closingChannelPage.getContent());
            return new PageImpl<>(closingChannelDTOS,closingChannelPage.getPageable(),closingChannelPage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<ClosingChannelDTO>> list() throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<ClosingChannel> closingChannelList;
            try{
                closingChannelList = closingChannelRepository.findAllByStatusTrue();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if(closingChannelList.isEmpty()){
                return Collections.emptyList();
            }

            return closingChannelMapper.listClosingChannelToListClosingChannelDTO(closingChannelList);
        });
    }

    @Override
    public CompletableFuture<ResponseDelete> delete(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            ClosingChannel closingChannel;
            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                closingChannel = closingChannelRepository.findByNameAndStatusTrue(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if (closingChannel==null){
                throw new BadRequestExceptions(Constants.ErrorClosingChannel);
            }
            try {
                closingChannel.setStatus(false);
                closingChannel.setUpdateDate(OffsetDateTime.now());
                closingChannel.setUser(user);
                closingChannel.setUserId(user.getId());
                closingChannelRepository.save(closingChannel);
                iAudit.save("DELETE_CLOSING_CHANNEL","CANAL DE CIERRE "+closingChannel.getName()+" DESACTIVADO.", closingChannel.getName(), user.getUsername());
                return ResponseDelete.builder()
                        .message(Constants.delete)
                        .code(200)
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
            ClosingChannel closingChannel;
            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                closingChannel = closingChannelRepository.findByNameAndStatusFalse(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(closingChannel==null){
                throw new BadRequestExceptions(Constants.ErrorClosingChannel);
            }
            try {
                closingChannel.setStatus(true);
                closingChannel.setUpdateDate(OffsetDateTime.now());
                closingChannel.setUser(user);
                closingChannel.setUserId(user.getId());
                closingChannelRepository.save(closingChannel);
                iAudit.save("ACTIVATE_CLOSING_CHANNEL","CANAL DE CIERRE "+closingChannel.getName()+" ACTIVADO.",closingChannel.getName(),user.getUsername());
                return ResponseSuccess.builder()
                        .message(Constants.update)
                        .code(200)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

}
