package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Size;
import com.proyect.masterdata.domain.SizeType;
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
import com.proyect.masterdata.services.ISize;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Date;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

@Service
@RequiredArgsConstructor
@Log4j2
public class SizeImpl implements ISize {

    private final SizeRepository sizeRepository;
    private final SizeMapper sizeMapper;
    private final UserRepository userRepository;
    private final SizeTypeRepository sizeTypeRepository;
    private final SizeRepositoryCustom sizeRepositoryCustom;

    @Override
    public ResponseSuccess save(String name, String sizeType, String tokenUser)
            throws BadRequestExceptions, InternalErrorExceptions {
        boolean existsUser;
        boolean existsSize;
        SizeType sizeTypeData;

        try {
            existsUser = userRepository.existsByUsernameAndStatusTrue(tokenUser.toUpperCase());
            existsSize = sizeRepository.existsByName(name.toUpperCase());
            sizeTypeData = sizeTypeRepository.findByName(sizeType.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (!existsUser) {
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }

        if (existsSize) {
            throw new BadRequestExceptions(Constants.ErrorSizeExists.toUpperCase());
        }

        if (sizeTypeData == null) {
            throw new BadRequestExceptions(Constants.ErrorSizeType.toUpperCase());
        }

        try {
            sizeRepository.save(Size.builder()
                    .name(name.toUpperCase())
                    .registrationDate(new Date(System.currentTimeMillis()))
                    .sizeType(sizeTypeData)
                    .sizeTypeId(sizeTypeData.getId())
                    .status(true)
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
    public ResponseSuccess saveAll(List<String> names, String sizeType, String tokenUser)
            throws BadRequestExceptions, InternalErrorExceptions {
        boolean existsUser;
        SizeType sizeTypeData;
        List<Size> sizes;

        try {
            existsUser = userRepository.existsByUsernameAndStatusTrue(tokenUser.toUpperCase());
            sizeTypeData = sizeTypeRepository.findByNameAndStatusTrue(sizeType.toUpperCase());
            sizes = sizeRepository.findByNameIn(names.stream().map(String::toUpperCase).toList());
        } catch (RuntimeException e) {
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (!existsUser) {
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if (sizeTypeData == null) {
            throw new BadRequestExceptions(Constants.ErrorSizeType.toUpperCase());
        }
        if (!sizes.isEmpty()) {
            throw new BadRequestExceptions(Constants.ErrorSizeList.toUpperCase());
        }

        try {
            List<Size> sizeSaves = names.stream().map(data -> Size.builder()
                    .tokenUser(tokenUser.toUpperCase())
                    .sizeType(sizeTypeData)
                    .sizeTypeId(sizeTypeData.getId())
                    .name(data.toUpperCase())
                    .build()).toList();
            sizeRepository.saveAll(sizeSaves);
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
    public ResponseDelete delete(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        boolean existsUser;
        Size size;

        try {
            existsUser = userRepository.existsByUsernameAndStatusTrue(tokenUser.toUpperCase());
            size = sizeRepository.findByNameAndStatusTrue(name.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (!existsUser) {
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if (size == null) {
            throw new BadRequestExceptions(Constants.ErrorSize.toUpperCase());
        }

        try {
            size.setStatus(false);
            size.setUpdateDate(new Date(System.currentTimeMillis()));
            sizeRepository.save(size);
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
    public List<SizeDTO> listSize() throws BadRequestExceptions {
        List<Size> sizes = new ArrayList<>();
        try {
            sizes = sizeRepository.findAllByStatusTrue();
        } catch (RuntimeException e) {
            log.error(e);
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
        if (sizes.isEmpty()) {
            return Collections.emptyList();
        }
        return sizeMapper.listSizeToListSizeDTO(sizes);
    }

    @Override
    public Page<SizeDTO> list(String name, String user, String sort,
            String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        Page<Size> sizePage;
        try {
            sizePage = sizeRepositoryCustom.searchForSize(name, user, sort, sortColumn,
                    pageNumber, pageSize, true);
        } catch (RuntimeException e) {
            log.error(e);
            throw new BadRequestExceptions(Constants.ResultsFound);
        }

        if (sizePage.isEmpty()) {
            return new PageImpl<>(Collections.emptyList());
        }
        return new PageImpl<>(sizeMapper.listSizeToListSizeDTO(sizePage.getContent()),
                sizePage.getPageable(), sizePage.getTotalElements());
    }

    public Page<SizeDTO> listStatusFalse(String name, String user, String sort,
            String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions {

        Page<Size> sizePage;

        try {
            sizePage = sizeRepositoryCustom.searchForSize(name, user, sort, sortColumn,
                    pageNumber, pageSize, false);
        } catch (RuntimeException e) {
            log.error(e);
            throw new BadRequestExceptions(Constants.ResultsFound);
        }

        if (sizePage.isEmpty()) {
            return new PageImpl<>(Collections.emptyList());
        }
        return new PageImpl<>(sizeMapper.listSizeToListSizeDTO(sizePage.getContent()),
                sizePage.getPageable(), sizePage.getTotalElements());
    }

    @Override
    public List<SizeDTO> findAllSizeTypeName(String nameSizeType) throws BadRequestExceptions {
        try {
            return sizeMapper.listSizeToListSizeDTO(
                    sizeRepository.findAllByStatusTrueAndSizeTypeName(nameSizeType.toUpperCase()));
        } catch (RuntimeException e) {
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }
}
