package com.proyect.masterdata.services.impl;

import java.util.Date;

import org.springframework.stereotype.Service;

import com.proyect.masterdata.domain.Brand;
import com.proyect.masterdata.domain.Model;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.BrandRepository;
import com.proyect.masterdata.repository.ModelRepository;
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

    @Override
    public ResponseSuccess save(String name, String brand, String user)
            throws InternalErrorExceptions, BadRequestExceptions {

        boolean existsUser;
        boolean existsModel;
        Brand brandData;

        try {
            existsUser = userRepository.existsByUser(user.toUpperCase());
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

        if(brandData==null){
            throw new BadRequestExceptions("Marca no existe");
        }

        try{
            modelRepository.save(Model.builder()
                .name(name.toUpperCase())
                .brand(brandData)
                .idBrand(brandData.getId())
                .dateRegistration(new Date(System.currentTimeMillis()))
                .status(true)
                .build()
            );
            return ResponseSuccess.builder()
            .code(200)
            .message(Constants.register)
            .build();
        }catch(RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        
    }

}
