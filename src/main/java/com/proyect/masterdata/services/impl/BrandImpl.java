package com.proyect.masterdata.services.impl;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import com.proyect.masterdata.domain.Brand;
import com.proyect.masterdata.dto.BrandDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.mapper.BrandMapper;
import com.proyect.masterdata.repository.BrandRepository;
import com.proyect.masterdata.repository.BrandRepositoryCustom;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IBrand;
import com.proyect.masterdata.utils.Constants;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import java.util.Collections;
import java.util.Date;
import java.util.List;

@Service
@RequiredArgsConstructor
@Log4j2
public class BrandImpl implements IBrand {

    private final UserRepository userRepository;
    private final BrandRepository brandRepository;
    private final BrandRepositoryCustom brandRepositoryCustom;
    private final BrandMapper brandMapper;

    @Override
    public ResponseSuccess save(String name, String user) throws InternalErrorExceptions, BadRequestExceptions {
        boolean existsUser;
        boolean existsBrand;

        try {
            existsUser = userRepository.existsByUsername(user.toUpperCase());
            existsBrand = brandRepository.existsByName(name.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (!existsUser) {
            throw new BadRequestExceptions("Usuario no existente");
        }

        if (existsBrand) {
            throw new BadRequestExceptions("Marca ya existente");
        }

        try {
            brandRepository.save(Brand.builder()
                    .name(name.toUpperCase())
                    .status(true)
                    .dateRegistration(new Date(System.currentTimeMillis()))
                    .user(user.toUpperCase())
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
    public ResponseSuccess saveAll(List<String> namesList, String user)
            throws InternalErrorExceptions, BadRequestExceptions {

        boolean existsUser;
        List<Brand> brandList;

        try {
            existsUser = userRepository.existsByUsername(user.toUpperCase());
            brandList = brandRepository.findByNameIn(namesList);
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (!existsUser) {
            throw new BadRequestExceptions("Usuario no existe");
        }

        if (!brandList.isEmpty()) {
            throw new BadRequestExceptions("Marca ya existente");
        }

        try {
            brandRepository.saveAll(namesList.stream().map(name -> Brand.builder()
                    .name(name.toUpperCase())
                    .status(true)
                    .dateRegistration(new Date(System.currentTimeMillis()))
                    .user(user.toUpperCase())
                    .build()).toList());
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
    public ResponseDelete delete(String name, String user) throws InternalErrorExceptions, BadRequestExceptions {
        boolean existsUser;
        Brand brand;
        try {
            existsUser = userRepository.existsByUsername(user.toUpperCase());
            brand = brandRepository.findByName(name.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (!existsUser) {
            throw new BadRequestExceptions("Usuario no existe");
        }

        if (brand == null) {
            throw new BadRequestExceptions("Marca no existe");
        }

        try {
            brand.setStatus(false);
            brand.setDateUpdate(new Date(System.currentTimeMillis()));
            brandRepository.save(brand);
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public Page<BrandDTO> list(String name, String user, String sort, String sortColumn, Integer pageNumber,
            Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions {

        Page<Brand> brandPage;

        try {
            brandPage = brandRepositoryCustom.searchForBrand(name, user, sort, sortColumn, pageNumber, pageSize, true);
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.ResultsFound);
        }

        if (brandPage.isEmpty()) {
            return new PageImpl<>(Collections.emptyList());
        }

        return new PageImpl<>(brandMapper.listBrandToListBrandDTO(brandPage.getContent()), brandPage.getPageable(),
                brandPage.getTotalElements());
    }

    @Override
    public Page<BrandDTO> listStatusFalse(String name, String user, String sort, String sortColumn, Integer pageNumber,
            Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions {

        Page<Brand> brandPage;

        try {
            brandPage = brandRepositoryCustom.searchForBrand(name, user, sort, sortColumn, pageNumber, pageSize, false);
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.ResultsFound);
        }

        if (brandPage.isEmpty()) {
            return new PageImpl<>(Collections.emptyList());
        }

        return new PageImpl<>(brandMapper.listBrandToListBrandDTO(brandPage.getContent()), brandPage.getPageable(),
                brandPage.getTotalElements());
    }

}
