package com.proyect.masterdata.services.impl;

import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.services.IAudit;
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
    private final IAudit iAudit;
    @Override
    public ResponseSuccess save(String name, String brand, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        boolean existsModel;
        Brand brandData;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            existsModel = modelRepository.existsByName(name.toUpperCase());
            brandData = brandRepository.findByName(brand.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (existsModel) {
            throw new BadRequestExceptions(Constants.ErrorModelExists);
        }

        if (brandData == null) {
            throw new BadRequestExceptions(Constants.ErrorBrand);
        }

        try {
            Model newModel = modelRepository.save(Model.builder()
                    .name(name.toUpperCase())
                    .brand(brandData)
                    .brandId(brandData.getId())
                    .client(user.getClient())
                    .clientId(user.getClientId())
                    .registrationDate(new Date(System.currentTimeMillis()))
                    .status(true)
                    .tokenUser(tokenUser.toUpperCase())
                    .build());
            iAudit.save("ADD_MODEL","ADD MODEL "+newModel.getName()+".",user.getUsername());
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
    public CompletableFuture<ResponseSuccess> saveAsync(String name, String brand, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            boolean existsModel;
            Brand brandData;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                existsModel = modelRepository.existsByName(name.toUpperCase());
                brandData = brandRepository.findByName(brand.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if (existsModel) {
                throw new BadRequestExceptions(Constants.ErrorModelExists);
            }

            if (brandData == null) {
                throw new BadRequestExceptions(Constants.ErrorBrand);
            }

            try {
                Model newModel = modelRepository.save(Model.builder()
                        .name(name.toUpperCase())
                        .brand(brandData)
                        .brandId(brandData.getId())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .status(true)
                        .tokenUser(tokenUser.toUpperCase())
                        .build());
                iAudit.save("ADD_MODEL","ADD MODEL "+newModel.getName()+".",user.getUsername());
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
    public CompletableFuture<ResponseDelete> delete(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Model modelData;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                modelData = modelRepository.findByNameAndStatusTrue(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user==null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if (modelData == null) {
                throw new BadRequestExceptions(Constants.ErrorModel);
            }

            try {
                modelData.setStatus(false);
                modelData.setUpdateDate(new Date(System.currentTimeMillis()));
                modelData.setTokenUser(user.getUsername());
                modelRepository.save(modelData);
                iAudit.save("DELETE_MODEL","DELETE MODEL "+modelData.getName()+".",user.getUsername());
                return ResponseDelete.builder()
                        .message(Constants.delete)
                        .code(200)
                        .build();
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> activate(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Model modelData;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                modelData = modelRepository.findByNameAndStatusFalse(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user==null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if (modelData == null) {
                throw new BadRequestExceptions(Constants.ErrorModel);
            }

            try {
                modelData.setStatus(true);
                modelData.setUpdateDate(new Date(System.currentTimeMillis()));
                modelData.setTokenUser(user.getUsername());
                modelRepository.save(modelData);
                iAudit.save("ACTIVATE_MODEL","ACTIVATE MODEL "+modelData.getName()+".",user.getUsername());
                return ResponseSuccess.builder()
                        .message(Constants.update)
                        .code(200)
                        .build();
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<Page<ModelDTO>> list(String name, String brand, String tokenUser, String sort, String columnSort,
            Integer pageNumber,
            Integer pageSize) {
        return CompletableFuture.supplyAsync(()->{
            Page<Model> pageModel;
            Brand brandData;
            Long clientId;

            if (brand != null) {
                brandData = brandRepository.findByName(brand.toUpperCase());
            } else {
                brandData = null;
            }

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase()).getClient().getId();
                pageModel = modelRepositoryCustom.searchForModel(name, brandData, clientId, sort, columnSort, pageNumber,
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
                    .user(model.getTokenUser())
                    .build()).toList();

            return new PageImpl<>(models, pageModel.getPageable(),
                    pageModel.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<Page<ModelDTO>> listStatusFalse(String name, String brand, String tokenUser, String sort, String columnSort,
            Integer pageNumber,
            Integer pageSize) {
        return CompletableFuture.supplyAsync(()->{
            Page<Model> pageModel;
            Brand brandData;
            Long clientId;

            if (brand != null) {
                brandData = brandRepository.findByName(brand.toUpperCase());
            } else {
                brandData = null;
            }

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase()).getClient().getId();
                pageModel = modelRepositoryCustom.searchForModel(name, brandData, clientId, sort, columnSort, pageNumber,
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
                    .user(model.getTokenUser())
                    .build()).toList();

            return new PageImpl<>(models, pageModel.getPageable(),
                    pageModel.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<ModelDTO>> listModels(String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<Model> models;
            Long clientId;

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                models = modelRepository.findAllByClientIdAndStatusTrue(clientId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(models.isEmpty()){
                return Collections.emptyList();
            }

            return models.stream().map(model -> ModelDTO.builder()
                    .name(model.getName())
                    .brand(model.getBrand().getName())
                    .user(model.getTokenUser())
                    .build()).toList();
        });
    }

    @Override
    public CompletableFuture<List<ModelDTO>> listModelsFalse(String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<Model> models;
            Long clientId;

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                models = modelRepository.findAllByClientIdAndStatusFalse(clientId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(models.isEmpty()){
                return Collections.emptyList();
            }

            return models.stream().map(model -> ModelDTO.builder()
                    .name(model.getName())
                    .brand(model.getBrand().getName())
                    .user(model.getTokenUser())
                    .build()).toList();
        });
    }

    @Override
    public CompletableFuture<List<ModelDTO>> listModelBrand(String user, String brand) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<Model> models;
            Long clientId;
            Long brandId;

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                brandId = brandRepository.findByName(brand.toUpperCase()).getId();
                models = modelRepository.findAllByClientIdAndBrandIdAndStatusTrue(clientId,brandId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(models.isEmpty()){
                return Collections.emptyList();
            }

            return models.stream().map(model -> ModelDTO.builder()
                    .name(model.getName())
                    .brand(model.getBrand().getName())
                    .user(model.getTokenUser())
                    .build()).toList();
        });
    }

}
