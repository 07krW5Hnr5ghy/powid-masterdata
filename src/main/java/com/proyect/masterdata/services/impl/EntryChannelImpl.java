package com.proyect.masterdata.services.impl;

import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import com.proyect.masterdata.repository.EntryChannelRepositoryCustom;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import com.proyect.masterdata.domain.EntryChannel;
import com.proyect.masterdata.dto.EntryChannelDTO;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.mapper.EntryChannelMapper;
import com.proyect.masterdata.repository.EntryChannelRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IEntryChannel;
import com.proyect.masterdata.utils.Constants;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class EntryChannelImpl implements IEntryChannel {

    private final UserRepository userRepository;
    private final EntryChannelRepository entryChannelRepository;
    private final EntryChannelMapper entryChannelMapper;
    private final EntryChannelRepositoryCustom entryChannelRepositoryCustom;

    @Override
    public CompletableFuture<ResponseSuccess> save(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            boolean existsUser;
            boolean existsEntryChannel;

            try {
                existsUser = userRepository.existsByUsernameAndStatusTrue(tokenUser.toUpperCase());
                existsEntryChannel = entryChannelRepository.existsByNameAndStatusTrue(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (!existsUser) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if (existsEntryChannel) {
                throw new BadRequestExceptions(Constants.ErrorEntryChannelExists);
            }

            try {
                entryChannelRepository.save(EntryChannel.builder()
                        .name(name.toUpperCase())
                        .status(true)
                        .registrationDate(new Date(System.currentTimeMillis()))
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
    public CompletableFuture<Page<EntryChannelDTO>> listEntryChannel(String name, String sort, String sortColumn, Integer pageNumber,
                                                                     Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<EntryChannel> entryChannelPage;

            try {
                entryChannelPage = entryChannelRepositoryCustom.searchEntryChannel(name,sort,sortColumn,pageNumber,pageSize,true);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (entryChannelPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }

            List<EntryChannelDTO> entryChannelDTOS = entryChannelMapper.listEntryChannelToListEntryChannelDTO(entryChannelPage.getContent());
            return new PageImpl<>(entryChannelDTOS,entryChannelPage.getPageable(),entryChannelPage.getTotalElements());
        });
    }
}
