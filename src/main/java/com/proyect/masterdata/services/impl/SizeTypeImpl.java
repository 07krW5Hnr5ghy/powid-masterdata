package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.SizeType;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.SizeTypeDTO;
import com.proyect.masterdata.dto.request.RequestSizeType;
import com.proyect.masterdata.dto.request.RequestSizeTypeSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.mapper.SizeTypeMapper;
import com.proyect.masterdata.repository.SizeTypeRepository;
import com.proyect.masterdata.repository.SizeTypeRepositoryCustom;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.ISizeType;
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
public class SizeTypeImpl implements ISizeType {
    private final SizeTypeRepository sizeTypeRepository;
    private final SizeTypeMapper sizeTypeMapper;
    private final UserRepository userRepository;
    private final SizeTypeRepositoryCustom sizeTypeRepositoryCustom;

    @Override
    public ResponseSuccess save(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        User datauser;
        SizeType sizeType;

        try {
            datauser = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            sizeType = sizeTypeRepository.findByNameAndStatusTrue(name.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (datauser == null) {
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if (sizeType != null) {
            throw new BadRequestExceptions(Constants.ErrorSizeTypeExists.toUpperCase());
        }

        try {
            sizeTypeRepository.save(SizeType.builder()
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
            throw new BadRequestExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public ResponseSuccess saveAll(List<String> names, String tokenUser)
            throws BadRequestExceptions, InternalErrorExceptions {

        User datauser;
        List<SizeType> sizeTypes;

        try {
            datauser = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            sizeTypes = sizeTypeRepository.findByNameIn(names.stream().map(String::toUpperCase).toList());
        } catch (RuntimeException e) {
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (datauser == null) {
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if (!sizeTypes.isEmpty()) {
            throw new BadRequestExceptions(Constants.ErrorSizeTypeList.toUpperCase());
        }

        try {
            List<SizeType> sizeTypeSaves = names.stream().map(data -> SizeType.builder()
                    .name(data.toUpperCase())
                    .tokenUser(tokenUser.toUpperCase())
                    .status(true)
                    .registrationDate(new Date(System.currentTimeMillis()))
                    .build()).toList();
            sizeTypeRepository.saveAll(sizeTypeSaves);
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
    @Transactional
    public ResponseDelete delete(String name, String user) throws BadRequestExceptions, InternalErrorExceptions {
        User datauser;
        SizeType sizeType;

        try {
            datauser = userRepository.findByUsernameAndStatusTrue(user.toUpperCase());
            sizeType = sizeTypeRepository.findByNameAndStatusTrue(name.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (datauser == null) {
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if (sizeType == null) {
            throw new BadRequestExceptions(Constants.ErrorSizeType.toUpperCase());
        }

        try {
            sizeType.setStatus(false);
            sizeType.setUpdateDate(new Date(System.currentTimeMillis()));
            sizeTypeRepository.save(sizeType);
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
    public List<SizeTypeDTO> listSizeType() throws BadRequestExceptions {
        List<SizeType> sizeTypes = new ArrayList<>();

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
    }

    @Override
    public Page<SizeTypeDTO> list(String name, String user, String sort, String sortColumn, Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {
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
    }

    @Override
    public Page<SizeTypeDTO> listStatusFalse(String name, String user, String sort, String sortColumn,
            Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
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
    }

}
