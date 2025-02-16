package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.SizeType;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.SizeTypeDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.mapper.SizeTypeMapper;
import com.proyect.masterdata.repository.SizeTypeRepository;
import com.proyect.masterdata.repository.SizeTypeRepositoryCustom;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.ISizeType;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.sql.Date;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class SizeTypeImpl implements ISizeType {
    private final SizeTypeRepository sizeTypeRepository;
    private final SizeTypeMapper sizeTypeMapper;
    private final UserRepository userRepository;
    private final SizeTypeRepositoryCustom sizeTypeRepositoryCustom;
    private final IAudit iAudit;
    @Override
    public ResponseSuccess save(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        User user;
        SizeType sizeType;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            sizeType = sizeTypeRepository.findByNameAndStatusTrue(name.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if (sizeType != null) {
            throw new BadRequestExceptions(Constants.ErrorSizeTypeExists.toUpperCase());
        }

        try {
            SizeType newSizeType = sizeTypeRepository.save(SizeType.builder()
                    .name(name.toUpperCase())
                    .status(true)
                    .registrationDate(OffsetDateTime.now())
                    .user(user).userId(user.getId())
                    .build());
            iAudit.save("ADD_SIZE_TYPE","TIPO DE TAMAÑO "+newSizeType.getName()+" CREADO.",newSizeType.getName(),user.getUsername());
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
    public CompletableFuture<ResponseSuccess> saveAsync(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            SizeType sizeType;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                sizeType = sizeTypeRepository.findByNameAndStatusTrue(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }
            if (sizeType != null) {
                throw new BadRequestExceptions(Constants.ErrorSizeTypeExists.toUpperCase());
            }

            try {
                SizeType newSizeType = sizeTypeRepository.save(SizeType.builder()
                        .name(name.toUpperCase())
                        .status(true)
                        .registrationDate(OffsetDateTime.now())
                        .user(user).userId(user.getId())
                        .build());
                iAudit.save("ADD_SIZE_TYPE","TIPO DE TAMAÑO "+newSizeType.getName()+" CREADO.",newSizeType.getName(),user.getUsername());
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
    public CompletableFuture<ResponseDelete> delete(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            SizeType sizeType;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                sizeType = sizeTypeRepository.findByNameAndStatusTrue(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e);
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }
            if (sizeType == null) {
                throw new BadRequestExceptions(Constants.ErrorSizeType.toUpperCase());
            }

            try {
                sizeType.setStatus(false);
                sizeType.setUpdateDate(OffsetDateTime.now());
                sizeType.setUser(user);
                sizeType.setUserId(user.getId());
                sizeTypeRepository.save(sizeType);
                iAudit.save("DELETE_SIZE_TYPE","TIPO DE MAÑANA "+sizeType.getName()+" DESACTIVADO.",sizeType.getName(),user.getUsername());
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
            SizeType sizeType;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                sizeType = sizeTypeRepository.findByNameAndStatusFalse(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e);
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }
            if (sizeType == null) {
                throw new BadRequestExceptions(Constants.ErrorSizeType.toUpperCase());
            }

            try {
                sizeType.setStatus(true);
                sizeType.setUpdateDate(OffsetDateTime.now());
                sizeType.setUser(user);
                sizeType.setUserId(user.getId());
                sizeTypeRepository.save(sizeType);
                iAudit.save("ACTIVATE_SIZE_TYPE","TIPO DE TAMAÑO "+sizeType.getName()+" ACTIVADO.",sizeType.getName(),user.getUsername());
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
    public CompletableFuture<List<SizeTypeDTO>> listSizeType() throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<SizeType> sizeTypes;

            try {
                sizeTypes = sizeTypeRepository.findAllByStatusTrue();
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (sizeTypes.isEmpty()) {
                return Collections.emptyList();
            }
            return sizeTypeMapper.listSizeTypeToListSizeTypeDTO(sizeTypes);
        });
    }

    @Override
    public CompletableFuture<List<SizeTypeDTO>> listSizeTypeFilter() throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<SizeType> sizeTypes;

            try {
                sizeTypes = sizeTypeRepository.findAll();
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (sizeTypes.isEmpty()) {
                return Collections.emptyList();
            }
            return sizeTypeMapper.listSizeTypeToListSizeTypeDTO(sizeTypes);
        });
    }

    @Override
    public CompletableFuture<Page<SizeTypeDTO>> list(String name, String user, String sort, String sortColumn, Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<SizeType> sizeTypePage;
            try {
                sizeTypePage = sizeTypeRepositoryCustom.searchForSizeType(name, user, sort, sortColumn, pageNumber,
                        pageSize, true);
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if (sizeTypePage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }
            return new PageImpl<>(sizeTypeMapper.listSizeTypeToListSizeTypeDTO(sizeTypePage.getContent()),
                    sizeTypePage.getPageable(), sizeTypePage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<Page<SizeTypeDTO>> listStatusFalse(String name, String user, String sort, String sortColumn,
            Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<SizeType> sizeTypePage;
            try {
                sizeTypePage = sizeTypeRepositoryCustom.searchForSizeType(name, user, sort, sortColumn, pageNumber,
                        pageSize, false);
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if (sizeTypePage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }
            return new PageImpl<>(sizeTypeMapper.listSizeTypeToListSizeTypeDTO(sizeTypePage.getContent()),
                    sizeTypePage.getPageable(), sizeTypePage.getTotalElements());
        });
    }

}
