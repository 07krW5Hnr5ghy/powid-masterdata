package com.proyect.masterdata.services.impl;

import java.util.Collections;
import java.util.Date;
import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import com.proyect.masterdata.domain.Brand;
import com.proyect.masterdata.domain.Model;
import com.proyect.masterdata.dto.ModelDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.mapper.ModelMapper;
import com.proyect.masterdata.repository.BrandRepository;
import com.proyect.masterdata.repository.ModelRepository;
import com.proyect.masterdata.repository.ModelRepositoryCustom;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IModel;
import com.proyect.masterdata.utils.Constants;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class ModelImpl implements IModel {

    private final ModelRepository modelRepository;
    private final UserRepository userRepository;
    private final BrandRepository brandRepository;
    private final ModelRepositoryCustom modelRepositoryCustom;
    private final ModelMapper modelMapper;

    @Override
    public ResponseSuccess save(String name, String brand, String user)
            throws InternalErrorExceptions, BadRequestExceptions {

        boolean existsUser;
        boolean existsModel;
        Brand brandData;

        try {
            existsUser = userRepository.existsByUsername(user.toUpperCase());
            existsModel = modelRepository.existsByName(name.toUpperCase());
            brandData = brandRepository.findByName(brand.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (!existsUser) {
            throw new BadRequestExceptions("Usuario no existe");
        }

        if (existsModel) {
            throw new BadRequestExceptions("Modelo ya existe");
        }

        if (brandData == null) {
            throw new BadRequestExceptions("Marca no existe");
        }

        try {
            modelRepository.save(Model.builder()
                    .name(name.toUpperCase())
                    .brand(brandData)
                    .idBrand(brandData.getId())
                    .dateRegistration(new Date(System.currentTimeMillis()))
                    .status(true)
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
    public ResponseSuccess saveAll(List<String> names, String brand, String user)
            throws InternalErrorExceptions, BadRequestExceptions {

        boolean existsUser;
        Brand brandData;
        List<Model> models;

        try {
            existsUser = userRepository.existsByUsername(user.toUpperCase());
            brandData = brandRepository.findByName(brand.toUpperCase());
            models = modelRepository.findByNameIn(names.stream().map(String::toUpperCase).toList());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (!existsUser) {
            throw new BadRequestExceptions("Usuario no existe");
        }

        if (brandData == null) {
            throw new BadRequestExceptions("Marca no existe");
        }

        if (!models.isEmpty()) {
            throw new BadRequestExceptions("Modelo ya existe");
        }

        try {
            modelRepository.saveAll(names.stream().map(model -> Model.builder()
                    .name(model.toUpperCase())
                    .brand(brandData)
                    .idBrand(brandData.getId())
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
        Model modelData;

        try {
            existsUser = userRepository.existsByUsername(user.toUpperCase());
            modelData = modelRepository.findByName(name.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (!existsUser) {
            throw new BadRequestExceptions("Usuario no existe");
        }

        if (modelData == null) {
            throw new BadRequestExceptions("Modelo no existe");
        }

        try {
            modelData.setStatus(false);
            modelData.setDateUpdate(new Date(System.currentTimeMillis()));
            modelRepository.save(modelData);
            return ResponseDelete.builder()
                    .message(Constants.delete)
                    .code(200)
                    .build();
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public Page<ModelDTO> list(String name, String brand, String user, String sort, String columnSort,
            Integer pageNumber,
            Integer pageSize) {

        Page<Model> pageModel;
        Brand brandData;

        if (brand != null) {
            brandData = brandRepository.findByName(brand.toUpperCase());
        } else {
            brandData = null;
        }

        try {
            pageModel = modelRepositoryCustom.searchForModel(name, brandData, user, sort, columnSort, pageNumber,
                    pageSize, true);
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.ResultsFound);
        }

        if (pageModel.isEmpty()) {
            return new PageImpl<>(Collections.emptyList());
        }

        List<ModelDTO> models = pageModel.getContent().stream().map(model -> ModelDTO.builder()
                .name(model.getName())
                .brand(model.getBrand().getName())
                .user(model.getUser())
                .build()).toList();

        return new PageImpl<>(models, pageModel.getPageable(),
                pageModel.getTotalElements());
    }

    @Override
    public Page<ModelDTO> listStatusFalse(String name, String brand, String user, String sort, String columnSort,
            Integer pageNumber,
            Integer pageSize) {

        Page<Model> pageModel;
        Brand brandData;

        if (brand != null) {
            brandData = brandRepository.findByName(brand.toUpperCase());
        } else {
            brandData = null;
        }

        try {
            pageModel = modelRepositoryCustom.searchForModel(name, brandData, user, sort, columnSort, pageNumber,
                    pageSize, false);
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.ResultsFound);
        }

        if (pageModel.isEmpty()) {
            return new PageImpl<>(Collections.emptyList());
        }

        List<ModelDTO> models = pageModel.getContent().stream().map(model -> ModelDTO.builder()
                .name(model.getName())
                .brand(model.getBrand().getName())
                .user(model.getUser())
                .build()).toList();

        return new PageImpl<>(models, pageModel.getPageable(),
                pageModel.getTotalElements());
    }

}
