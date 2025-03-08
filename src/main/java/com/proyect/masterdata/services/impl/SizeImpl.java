package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Size;
import com.proyect.masterdata.domain.SizeType;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.SizeDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.mapper.SizeMapper;
import com.proyect.masterdata.repository.SizeRepository;
import com.proyect.masterdata.repository.SizeRepositoryCustom;
import com.proyect.masterdata.repository.SizeTypeRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.ISize;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.OffsetDateTime;
import java.util.Date;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class SizeImpl implements ISize {

    private final SizeRepository sizeRepository;
    private final SizeMapper sizeMapper;
    private final UserRepository userRepository;
    private final SizeTypeRepository sizeTypeRepository;
    private final SizeRepositoryCustom sizeRepositoryCustom;
    private final IAudit iAudit;
    @Override
    public ResponseSuccess save(String name, String sizeType, String tokenUser)
            throws BadRequestExceptions, InternalErrorExceptions {
        User user;
        Size size;
        SizeType sizeTypeData;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            sizeTypeData = sizeTypeRepository.findByName(sizeType.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user==null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }else{
            size = sizeRepository.findByNameAndClientId(name.toUpperCase(),user.getClientId());
        }

        if (size!=null) {
            throw new BadRequestExceptions(Constants.ErrorSizeExists);
        }

        if (sizeTypeData == null) {
            throw new BadRequestExceptions(Constants.ErrorSizeType);
        }

        try {
            Size newSize = sizeRepository.save(Size.builder()
                    .name(name.toUpperCase())
                    .registrationDate(OffsetDateTime.now())
                    .sizeType(sizeTypeData)
                    .sizeTypeId(sizeTypeData.getId())
                    .status(true)
                    .user(user).userId(user.getId())
                            .client(user.getClient())
                            .clientId(user.getClientId())
                    .build());
            iAudit.save("ADD_SIZE","TAMAÑO "+newSize.getName()+" CREADO.",newSize.getName(),user.getUsername());
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
    public CompletableFuture<ResponseSuccess> saveAsync(String name, String sizeType, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Size size;
            SizeType sizeTypeData;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                sizeTypeData = sizeTypeRepository.findByName(sizeType.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user==null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                size = sizeRepository.findByNameAndClientId(name.toUpperCase(),user.getClientId());
            }

            if (size==null) {
                throw new BadRequestExceptions(Constants.ErrorSizeExists);
            }

            if (sizeTypeData == null) {
                throw new BadRequestExceptions(Constants.ErrorSizeType);
            }

            try {
                Size newSize = sizeRepository.save(Size.builder()
                        .name(name.toUpperCase())
                        .registrationDate(OffsetDateTime.now())
                        .sizeType(sizeTypeData)
                        .sizeTypeId(sizeTypeData.getId())
                        .status(true)
                        .user(user).userId(user.getId())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .build());
                iAudit.save("ADD_SIZE","TAMAÑO "+newSize.getName()+" CREADO.",newSize.getName(),user.getUsername());
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
            Size size;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e);
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user==null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }else{
                size = sizeRepository.findByNameAndStatusTrueAndClientId(name.toUpperCase(),user.getClientId());
            }
            if (size == null) {
                throw new BadRequestExceptions(Constants.ErrorSize.toUpperCase());
            }

            try {
                size.setStatus(false);
                size.setUpdateDate(OffsetDateTime.now());
                size.setUser(user);
                size.setUserId(user.getId());
                sizeRepository.save(size);
                iAudit.save("DELETE_SIZE","TAMAÑO "+size.getName()+" DESACTIVADO.",size.getName(),user.getUsername());
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
            Size size;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e);
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user==null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }else{
                size = sizeRepository.findByNameAndStatusFalseAndClientId(name.toUpperCase(),user.getClientId());
            }
            if (size == null) {
                throw new BadRequestExceptions(Constants.ErrorSize.toUpperCase());
            }

            try {
                size.setStatus(true);
                size.setUpdateDate(OffsetDateTime.now());
                sizeRepository.save(size);
                iAudit.save("ACTIVATE_SIZE","TAMAÑO "+size.getName()+" ACTIVADO.",size.getName(),user.getUsername());
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
    public CompletableFuture<List<SizeDTO>> listSize(String username) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<Size> sizes;
            User user;

            try {
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                sizes = sizeRepository.findAllByStatusTrueAndClientId(user.getClientId());
            }

            if (sizes.isEmpty()) {
                return Collections.emptyList();
            }

            return sizes.stream().map(size -> SizeDTO.builder()
                    .id(size.getId())
                    .name(size.getName())
                    .sizeType(size.getSizeType().getName())
                    .registrationDate(size.getRegistrationDate())
                    .updateDate(size.getUpdateDate())
                    .status(size.getStatus())
                    .build()).toList();
        });
    }

    @Override
    public CompletableFuture<Page<SizeDTO>> list(String name, String user, String sort,
            String sortColumn, Integer pageNumber, Integer pageSize,Boolean status) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<Size> sizePage;

            try {
                sizePage = sizeRepositoryCustom.searchForSize(name, user, sort, sortColumn,
                        pageNumber, pageSize, status);
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (sizePage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }

            List<SizeDTO> sizeDTOs = sizePage.getContent().stream().map(size -> SizeDTO.builder()
                    .id(size.getId())
                    .name(size.getName())
                    .sizeType(size.getSizeType().getName())
                    .registrationDate(size.getRegistrationDate())
                    .updateDate(size.getUpdateDate())
                    .status(size.getStatus())
                    .sizeType(size.getSizeType().getName())
                    .build()).toList();

            return new PageImpl<>(sizeDTOs,
                    sizePage.getPageable(), sizePage.getTotalElements());
        });
    }
    @Override
    public CompletableFuture<List<SizeDTO>> findAllSizeTypeName(String nameSizeType,String username) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            try {
                User user;
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());

                if(user==null){
                    throw new BadRequestExceptions(Constants.ErrorUser);
                }

                List<Size> sizes = sizeRepository.findAllByStatusTrueAndSizeTypeNameAndClientId(nameSizeType.toUpperCase(),user.getClientId());

                return sizes.stream().map(size -> SizeDTO.builder()
                        .id(size.getId())
                        .name(size.getName())
                        .sizeType(size.getSizeType().getName())
                        .registrationDate(size.getRegistrationDate())
                        .updateDate(size.getUpdateDate())
                        .status(size.getStatus())
                        .build()).toList();
            } catch (RuntimeException e) {
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
        });
    }

    @Override
    public CompletableFuture<List<SizeDTO>> listFilter(String username) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<Size> sizes;
            User user;
            try {
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                sizes = sizeRepository.findAllByClientId(user.getClientId());
            }

            if (sizes.isEmpty()) {
                return Collections.emptyList();
            }

            return sizes.stream().map(size -> SizeDTO.builder()
                    .id(size.getId())
                    .name(size.getName())
                    .sizeType(size.getSizeType().getName())
                    .registrationDate(size.getRegistrationDate())
                    .updateDate(size.getUpdateDate())
                    .status(size.getStatus())
                    .build()).toList();
        });
    }
}
