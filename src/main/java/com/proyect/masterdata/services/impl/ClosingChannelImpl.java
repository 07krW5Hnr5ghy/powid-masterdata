package com.proyect.masterdata.services.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import com.proyect.masterdata.repository.ClosingChannelRepositoryCustom;
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

    @Override
    public CompletableFuture<ResponseSuccess> save(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            boolean existsUser;
            boolean existsClosingChannel;

            try {
                existsUser = userRepository.existsByUsernameAndStatusTrue(tokenUser.toUpperCase());
                existsClosingChannel = closingChannelRepository.existsByNameAndStatusTrue(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }

            if (!existsUser) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if (existsClosingChannel) {
                throw new BadRequestExceptions(Constants.ErrorClosingChannelExists);
            }

            try {
                closingChannelRepository.save(ClosingChannel.builder()
                        .name(name.toUpperCase())
                        .status(true)
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .updateDate(new Date(System.currentTimeMillis()))
                        .tokenUser(tokenUser.toUpperCase())
                        .build());

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
    public CompletableFuture<Page<ClosingChannelDTO>> listClosingChannel(String name, String sort, String sortColumn, Integer pageNumber,
                                                                         Integer pageSize) throws InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<ClosingChannel> closingChannelPage;
            try {
                closingChannelPage = closingChannelRepositoryCustom.searchForClosingChannel(name, sort, sortColumn, pageNumber, pageSize,true);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if(closingChannelPage.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }

            List<ClosingChannelDTO> closingChannelDTOS = closingChannelMapper.listClosingChannelToListClosindChannelDTO(closingChannelPage.getContent());
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

            return closingChannelMapper.listClosingChannelToListClosindChannelDTO(closingChannelList);
        });
    }

}
