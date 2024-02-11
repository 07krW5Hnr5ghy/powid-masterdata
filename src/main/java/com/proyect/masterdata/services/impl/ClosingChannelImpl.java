package com.proyect.masterdata.services.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;

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

    @Override
    public ResponseSuccess save(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {

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
    }

    @Override
    public List<ClosingChannelDTO> listClosingChannel() throws InternalErrorExceptions {

        List<ClosingChannel> closingChannels = new ArrayList<>();

        try {
            closingChannels = closingChannelRepository.findAllByStatusTrue();
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.ResultsFound);
        }

        if (closingChannels.isEmpty()) {
            return Collections.emptyList();
        }

        return closingChannelMapper.listClosingChannelToListClosindChannelDTO(closingChannels);
    }

}
