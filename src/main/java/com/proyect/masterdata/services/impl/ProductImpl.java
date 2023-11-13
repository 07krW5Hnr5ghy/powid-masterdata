package com.proyect.masterdata.services.impl;

import java.util.Date;

import org.springframework.stereotype.Service;

import com.proyect.masterdata.domain.Category;
import com.proyect.masterdata.domain.Color;
import com.proyect.masterdata.domain.Model;
import com.proyect.masterdata.domain.Product;
import com.proyect.masterdata.domain.Size;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.CategoryRepository;
import com.proyect.masterdata.repository.ColorRepository;
import com.proyect.masterdata.repository.ModelRepository;
import com.proyect.masterdata.repository.ProductRepository;
import com.proyect.masterdata.repository.SizeRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IProduct;
import com.proyect.masterdata.utils.Constants;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class ProductImpl implements IProduct {

    private final ProductRepository productRepository;
    private final UserRepository userRepository;
    private final ModelRepository modelRepository;
    private final SizeRepository sizeRepository;
    private final CategoryRepository categoryRepository;
    private final ColorRepository colorRepository;

    @Override
    public ResponseSuccess save(String sku, String model, String size, String category, String color, String user)
            throws InternalErrorExceptions, BadRequestExceptions {

        boolean existsUser;
        boolean existsProduct;
        Model modelData;
        Size sizeData;
        Category categoryData;
        Color colorData;

        try {
            existsUser = userRepository.existsByUser(user.toUpperCase());
            existsProduct = productRepository.existsBySku(sku.toUpperCase());
            modelData = modelRepository.findByName(model.toUpperCase());
            sizeData = sizeRepository.findByNameAndStatusTrue(size.toUpperCase());
            categoryData = categoryRepository.findByNameAndStatusTrue(category.toUpperCase());
            colorData = colorRepository.findByNameAndStatusTrue(color.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (!existsUser) {
            throw new BadRequestExceptions("Usuario no existe");
        }

        if (existsProduct) {
            throw new BadRequestExceptions("Producto ya existe");
        }

        if (modelData == null) {
            throw new BadRequestExceptions("Modelo no existe");
        }

        if (sizeData == null) {
            throw new BadRequestExceptions("Talla no existe");
        }

        if (categoryData == null) {
            throw new BadRequestExceptions("Categoria no existe");
        }

        if (colorData == null) {
            throw new BadRequestExceptions("Color no existe");
        }

        try {
            productRepository.save(Product.builder()
                    .sku(sku.toUpperCase())
                    .model(modelData)
                    .idModel(modelData.getId())
                    .size(sizeData)
                    .idSize(sizeData.getId())
                    .category(categoryData)
                    .idCategory(categoryData.getId())
                    .color(colorData)
                    .idColor(colorData.getId())
                    .user(user.toUpperCase())
                    .status(existsProduct)
                    .dateRegistration(new Date(System.currentTimeMillis()))
                    .build());
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e) {
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

    }

}
