package com.proyect.masterdata.services.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.request.RequestModel;
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
    public ResponseSuccess save(RequestModel requestModel)
            throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        Model model;
        Model modelName;
        Brand brandData;

        try {
            user = userRepository.findByUsernameAndStatusTrue(requestModel.getTokenUser().toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }else{
            brandData = brandRepository.findByNameAndClientId(requestModel.getBrand().toUpperCase(),user.getClientId());
        }

        if (brandData == null) {
            throw new BadRequestExceptions(Constants.ErrorBrand);
        }else{
            model = modelRepository.findBySkuAndClientId(
                    requestModel.getSku().toUpperCase(),
                    user.getClientId()
            );
            modelName = modelRepository.findByNameAndClientId(
                    requestModel.getName().toUpperCase(),
                    user.getClientId()
            );
        }

        if (model != null || modelName != null) {
            throw new BadRequestExceptions(Constants.ErrorModelExists);
        }

        try {
            Model newModel = modelRepository.save(Model.builder()
                    .name(requestModel.getName().toUpperCase())
                    .brand(brandData)
                    .brandId(brandData.getId())
                    .client(user.getClient())
                    .clientId(user.getClientId())
                    .registrationDate(new Date(System.currentTimeMillis()))
                    .status(true)
                    .sku(requestModel.getSku().toUpperCase())
                    .tokenUser(user.getUsername())
                    .build());
            iAudit.save("ADD_MODEL","MODELO "+newModel.getName()+" CREADO.",newModel.getName(),user.getUsername());
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
    public CompletableFuture<ResponseSuccess> saveAsync(RequestModel requestModel)
            throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Model model;
            Brand brandData;
            Model modelName;

            try {
                user = userRepository.findByUsernameAndStatusTrue(requestModel.getTokenUser().toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                brandData = brandRepository.findByNameAndClientId(requestModel.getBrand().toUpperCase(),user.getClientId());
            }

            if (brandData == null) {
                throw new BadRequestExceptions(Constants.ErrorBrand);
            }else{
                model = modelRepository.findBySkuAndClientId(
                        requestModel.getSku().toUpperCase(),
                        user.getClientId()
                );
                modelName = modelRepository.findByNameAndClientId(
                        requestModel.getName().toUpperCase(),
                        user.getClientId()
                );
            }

            if (model != null || modelName != null) {
                throw new BadRequestExceptions(Constants.ErrorModelExists);
            }

            try {
                Model newModel = modelRepository.save(Model.builder()
                        .name(requestModel.getName().toUpperCase())
                        .brand(brandData)
                        .sku(requestModel.getSku().toUpperCase())
                        .brandId(brandData.getId())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .status(true)
                        .tokenUser(user.getUsername())
                        .build());
                iAudit.save("ADD_MODEL","MODELO "+newModel.getName()+" CREADO.",newModel.getName(),user.getUsername());
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
    public CompletableFuture<ResponseDelete> delete(String sku, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Model modelData;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user==null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                modelData = modelRepository.findBySkuAndClientIdAndStatusTrue(sku.toUpperCase(),user.getClientId());
            }

            if (modelData == null) {
                throw new BadRequestExceptions(Constants.ErrorModel);
            }

            try {
                modelData.setStatus(false);
                modelData.setUpdateDate(new Date(System.currentTimeMillis()));
                modelData.setTokenUser(user.getUsername());
                modelRepository.save(modelData);
                iAudit.save("DELETE_MODEL","MODELO "+modelData.getName()+" DESACTIVADO.",modelData.getName(),user.getUsername());
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
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user==null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                modelData = modelRepository.findBySkuAndClientIdAndStatusFalse(name.toUpperCase(),user.getClientId());
            }

            if (modelData == null) {
                throw new BadRequestExceptions(Constants.ErrorModel);
            }

            try {
                modelData.setStatus(true);
                modelData.setUpdateDate(new Date(System.currentTimeMillis()));
                modelData.setTokenUser(user.getUsername());
                modelRepository.save(modelData);
                iAudit.save("ACTIVATE_MODEL","MODELO "+modelData.getName()+" ACTIVADO.",modelData.getName(),user.getUsername());
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
    public CompletableFuture<Page<ModelDTO>> list(
            String user,
            List<String> names,
            List<String> brands,
            Date registrationStartDate,
            Date registrationEndDate,
            Date updateStartDate,
            Date updateEndDate,
            String sort,
            String columnSort,
            Integer pageNumber,
            Integer pageSize) {
        return CompletableFuture.supplyAsync(()->{
            Page<Model> pageModel;
            List<String> modelsUppercase;
            List<Long> brandIds;
            Long clientId;

            if(names != null && !names.isEmpty()){
                modelsUppercase = names.stream().map(String::toUpperCase).toList();
            }else{
                modelsUppercase = new ArrayList<>();
            }



            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClient().getId();
                if (brands != null && !brands.isEmpty()) {
                    brandIds = brandRepository.findByClientIdAndNameIn(
                            clientId,
                            brands.stream().map(String::toUpperCase).toList()
                    ).stream().map(Brand::getId).toList();
                } else {
                    brandIds = new ArrayList<>();
                }
                pageModel = modelRepositoryCustom.searchForModel(
                        clientId,
                        modelsUppercase,
                        brandIds,
                        registrationStartDate,
                        registrationEndDate,
                        updateStartDate,
                        updateEndDate,
                        sort,
                        columnSort,
                        pageNumber,
                        pageSize,
                        true);
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
                    .sku(model.getSku())
                    .user(model.getTokenUser())
                    .registrationDate(model.getRegistrationDate())
                    .updateDate(model.getUpdateDate())
                    .build()).toList();

            return new PageImpl<>(models, pageModel.getPageable(),
                    pageModel.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<Page<ModelDTO>> listStatusFalse(
            String user,
            List<String> names,
            List<String> brands,
            Date registrationStartDate,
            Date registrationEndDate,
            Date updateStartDate,
            Date updateEndDate,
            String sort,
            String columnSort,
            Integer pageNumber,
            Integer pageSize) {
        return CompletableFuture.supplyAsync(()->{
            Page<Model> pageModel;
            List<String> modelsUppercase;
            List<Long> brandIds;
            Long clientId;

            if(names != null && !names.isEmpty()){
                modelsUppercase = names.stream().map(String::toUpperCase).toList();
            }else{
                modelsUppercase = new ArrayList<>();
            }

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClient().getId();
                if (brands != null && !brands.isEmpty()) {
                    brandIds = brandRepository.findByClientIdAndNameIn(
                            clientId,
                            brands.stream().map(String::toUpperCase).toList()
                    ).stream().map(Brand::getId).toList();
                } else {
                    brandIds = new ArrayList<>();
                }
                pageModel = modelRepositoryCustom.searchForModel(
                        clientId,
                        modelsUppercase,
                        brandIds,
                        registrationStartDate,
                        registrationEndDate,
                        updateStartDate,
                        updateEndDate,
                        sort,
                        columnSort,
                        pageNumber,
                        pageSize,
                        false);
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
                    .sku(model.getSku())
                    .user(model.getTokenUser())
                    .registrationDate(model.getRegistrationDate())
                    .updateDate(model.getUpdateDate())
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
                    .sku(model.getSku().toUpperCase())
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
                    .sku(model.getSku())
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
                brandId = brandRepository.findByNameAndClientId(brand.toUpperCase(),clientId).getId();
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
                    .sku(model.getSku())
                    .user(model.getTokenUser())
                    .build()).toList();
        });
    }

    @Override
    public CompletableFuture<List<ModelDTO>> listFilter(String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<Model> models;
            Long clientId;

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                models = modelRepository.findAllByClientId(clientId);
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
                    .sku(model.getSku())
                    .build()).toList();
        });
    }

}
