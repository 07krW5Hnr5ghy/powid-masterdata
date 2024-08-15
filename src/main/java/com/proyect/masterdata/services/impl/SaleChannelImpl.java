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
import com.proyect.masterdata.services.IAudit;
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
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class SaleChannelImpl implements ISaleChannel {
    private final SaleChannelRepository saleChannelRepository;
    private final SaleChannelMapper saleChannelMapper;
    private final UserRepository userRepository;
    private final SaleChannelRepositoryCustom saleChannelRepositoryCustom;
    private final IAudit iAudit;
    @Override
    public ResponseSuccess save(String name, String user) throws BadRequestExceptions, InternalErrorExceptions {
        User datauser;
        SaleChannel saleChannel;

        try {
            datauser = userRepository.findByUsernameAndStatusTrue(user.toUpperCase());
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
            SaleChannel newSaleChannel = saleChannelRepository.save(saleChannelMapper.saleChannelToName(RequestSaleChannelSave.builder()
                    .name(name.toUpperCase()).user(datauser.getUsername().toUpperCase()).build()));
            iAudit.save("ADD_SALE_CHANNEL","CANAL DE VENTA "+newSaleChannel.getName()+" CREADO.",newSaleChannel.getName(),datauser.getUsername());
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
    public CompletableFuture<ResponseSuccess> saveAsync(String name, String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User datauser;
            SaleChannel saleChannel;

            try {
                datauser = userRepository.findByUsernameAndStatusTrue(user.toUpperCase());
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
                SaleChannel newSaleChannel = saleChannelRepository.save(saleChannelMapper.saleChannelToName(RequestSaleChannelSave.builder()
                        .name(name.toUpperCase()).user(datauser.getUsername().toUpperCase()).build()));
                iAudit.save("ADD_SALE_CHANNEL","CANAL DE VENTA "+newSaleChannel.getName()+" CREADO.",newSaleChannel.getName(),datauser.getUsername());
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
    public CompletableFuture<ResponseDelete> delete(String name, String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User datauser;
            SaleChannel saleChannel;
            try {
                datauser = userRepository.findByUsernameAndStatusTrue(user.toUpperCase());
                saleChannel = saleChannelRepository.findByNameAndStatusTrue(name.toUpperCase());
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
                saleChannel.setRegistrationDate(new Date(System.currentTimeMillis()));
                saleChannel.setTokenUser(datauser.getUsername());
                saleChannelRepository.save(saleChannel);
                iAudit.save("DELETE_SALE_CHANNEL","CANAL DE VENTA "+saleChannel.getName()+" DESACTIVADO.",saleChannel.getName(),datauser.getUsername());
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
    public CompletableFuture<List<SaleChannelDTO>> listSaleChannel() throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
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
        });
    }

    @Override
    public CompletableFuture<Page<SaleChannelDTO>> list(String name, String user, String sort, String sortColumn, Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
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
        });
    }

    @Override
    public CompletableFuture<Page<SaleChannelDTO>> listStatusFalse(String name, String user, String sort, String sortColumn,
            Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
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
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> activate(String name, String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User datauser;
            SaleChannel saleChannel;
            try {
                datauser = userRepository.findByUsernameAndStatusTrue(user.toUpperCase());
                saleChannel = saleChannelRepository.findByNameAndStatusFalse(name.toUpperCase());
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
                saleChannel.setStatus(true);
                saleChannel.setRegistrationDate(new Date(System.currentTimeMillis()));
                saleChannel.setTokenUser(datauser.getUsername());
                saleChannelRepository.save(saleChannel);
                iAudit.save("ACTIVATE_SALE_CHANNEL","CANAL DE VENTA "+saleChannel.getName()+" ACTIVADO.",saleChannel.getName(),datauser.getUsername());
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

}
