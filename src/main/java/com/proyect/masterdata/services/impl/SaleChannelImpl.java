package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.SaleChannel;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.SaleChannelDTO;
import com.proyect.masterdata.dto.request.RequestSaleChannel;
import com.proyect.masterdata.dto.request.RequestSaleChannelSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.mapper.SaleChannelMapper;
import com.proyect.masterdata.repository.SaleChannelRepository;
import com.proyect.masterdata.repository.SaleChannelRepositoryCustom;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.ISaleChannel;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.sql.Date;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

@Service
@RequiredArgsConstructor
@Log4j2
public class SaleChannelImpl implements ISaleChannel {
    private final SaleChannelRepository saleChannelRepository;
    private final SaleChannelMapper saleChannelMapper;
    private final UserRepository userRepository;
    private final SaleChannelRepositoryCustom saleChannelRepositoryCustom;

    @Override
    public ResponseSuccess save(String name, String user) throws BadRequestExceptions, InternalErrorExceptions {
        User datauser;
        SaleChannel saleChannel;

        try {
            datauser = userRepository.findById(user.toUpperCase()).orElse(null);
            saleChannel = saleChannelRepository.findByNameAndStatusTrue(name.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (datauser == null) {
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if (saleChannel != null) {
            throw new BadRequestExceptions(Constants.ErrorSaleChannelExists.toUpperCase());
        }

        try {
            saleChannelRepository.save(saleChannelMapper.saleChannelToName(RequestSaleChannelSave.builder()
                    .name(name.toUpperCase()).user(datauser.getUsername().toUpperCase()).build()));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public ResponseSuccess saveAll(List<String> names, String user)
            throws BadRequestExceptions, InternalErrorExceptions {
        User datauser;
        List<SaleChannel> saleChannels;

        try {
            datauser = userRepository.findById(user.toUpperCase()).orElse(null);
            saleChannels = saleChannelRepository.findByNameIn(names.stream().map(String::toUpperCase).toList());
        } catch (RuntimeException e) {
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (datauser == null) {
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if (!saleChannels.isEmpty()) {
            throw new BadRequestExceptions(Constants.ErrorSaleChannelList.toUpperCase());
        }

        try {
            List<RequestSaleChannelSave> saleChannelSaves = names.stream().map(data -> RequestSaleChannelSave.builder()
                    .user(user.toUpperCase())
                    .name(data.toUpperCase())
                    .build()).toList();
            saleChannelRepository.saveAll(saleChannelMapper.listSaleChannelToListName(saleChannelSaves));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e) {
            log.error(e);
            throw new BadRequestExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public SaleChannelDTO update(RequestSaleChannel requestSaleChannel)
            throws BadRequestExceptions, InternalErrorExceptions {
        User datauser;
        SaleChannel saleChannel;

        try {
            datauser = userRepository.findById(requestSaleChannel.getUser().toUpperCase()).orElse(null);
            saleChannel = saleChannelRepository.findById(requestSaleChannel.getCode()).orElse(null);
        } catch (RuntimeException e) {
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (datauser == null) {
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if (saleChannel == null) {
            throw new BadRequestExceptions(Constants.ErrorSaleChannel.toUpperCase());
        }

        saleChannel.setName(requestSaleChannel.getName().toUpperCase());
        saleChannel.setUser(datauser.getUsername().toUpperCase());
        saleChannel.setStatus(requestSaleChannel.isStatus());
        saleChannel.setDateRegistration(new Date(System.currentTimeMillis()));

        try {
            return saleChannelMapper.saleChannelToSaleChannelDTO(saleChannelRepository.save(saleChannel));
        } catch (RuntimeException e) {
            log.error(e);
            throw new BadRequestExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    @Transactional
    public ResponseDelete delete(Long code, String user) throws BadRequestExceptions, InternalErrorExceptions {
        User datauser;
        SaleChannel saleChannel;

        try {
            datauser = userRepository.findById(user.toUpperCase()).orElse(null);
            saleChannel = saleChannelRepository.findById(code).orElse(null);
        } catch (RuntimeException e) {
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (datauser == null) {
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if (saleChannel == null) {
            throw new BadRequestExceptions(Constants.ErrorSaleChannel.toUpperCase());
        }

        try {
            saleChannel.setStatus(false);
            saleChannel.setDateRegistration(new Date(System.currentTimeMillis()));
            saleChannelRepository.save(saleChannel);
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e) {
            log.error(e);
            throw new BadRequestExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public List<SaleChannelDTO> listSaleChannel() throws BadRequestExceptions {
        List<SaleChannel> saleChannels = new ArrayList<>();
        try {
            saleChannels = saleChannelRepository.findAllByStatusTrue();
        } catch (RuntimeException e) {
            log.error(e);
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
        if (saleChannels.isEmpty()) {
            return Collections.emptyList();
        }
        return saleChannelMapper.listSaleChannelToListSaleChannelDTO(saleChannels);
    }

    @Override
    public Page<SaleChannelDTO> list(String name, String user, String sort, String sortColumn, Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {
        Page<SaleChannel> saleChannelPage;
        try {
            saleChannelPage = saleChannelRepositoryCustom.searchForSaleChannel(name, user, sort, sortColumn, pageNumber,
                    pageSize, true);
        } catch (RuntimeException e) {
            log.error(e);
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
        if (saleChannelPage.isEmpty()) {
            return new PageImpl<>(Collections.emptyList());
        }
        return new PageImpl<>(saleChannelMapper.listSaleChannelToListSaleChannelDTO(saleChannelPage.getContent()),
                saleChannelPage.getPageable(), saleChannelPage.getTotalElements());
    }

    @Override
    public Page<SaleChannelDTO> listStatusFalse(String name, String user, String sort, String sortColumn,
            Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        Page<SaleChannel> saleChannelPage;
        try {
            saleChannelPage = saleChannelRepositoryCustom.searchForSaleChannel(name, user, sort, sortColumn, pageNumber,
                    pageSize, false);
        } catch (RuntimeException e) {
            log.error(e);
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
        if (saleChannelPage.isEmpty()) {
            return new PageImpl<>(Collections.emptyList());
        }
        return new PageImpl<>(saleChannelMapper.listSaleChannelToListSaleChannelDTO(saleChannelPage.getContent()),
                saleChannelPage.getPageable(), saleChannelPage.getTotalElements());
    }

    @Override
    public SaleChannelDTO findByCode(Long code) throws BadRequestExceptions {
        try {
            return saleChannelMapper.saleChannelToSaleChannelDTO(saleChannelRepository.findByIdAndStatusTrue(code));
        } catch (RuntimeException e) {
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

}
