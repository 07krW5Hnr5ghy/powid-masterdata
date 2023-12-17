package com.proyect.masterdata.services.impl;

import java.util.Collections;
import java.util.Date;
import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import com.proyect.masterdata.domain.Category;
import com.proyect.masterdata.domain.Client;
import com.proyect.masterdata.domain.Color;
import com.proyect.masterdata.domain.Model;
import com.proyect.masterdata.domain.Product;
import com.proyect.masterdata.domain.Size;
import com.proyect.masterdata.dto.ProductDTO;
import com.proyect.masterdata.dto.request.RequestProductSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.CategoryRepository;
import com.proyect.masterdata.repository.ColorRepository;
import com.proyect.masterdata.repository.ModelRepository;
import com.proyect.masterdata.repository.ProductRepository;
import com.proyect.masterdata.repository.ProductRepositoryCustom;
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
    private final ProductRepositoryCustom productRepositoryCustom;

    @Override
    public ResponseSuccess save(RequestProductSave product, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {

        boolean existsUser;
        boolean existsProduct;
        Model modelData;
        Size sizeData;
        Category categoryData;
        Color colorData;

        try {
            existsUser = userRepository.existsByUsernameAndStatusTrue(tokenUser.toUpperCase());
            existsProduct = productRepository.existsBySku(product.getSku().toUpperCase());
            modelData = modelRepository.findByName(product.getModel().toUpperCase());
            sizeData = sizeRepository.findByNameAndStatusTrue(product.getSize().toUpperCase());
            categoryData = categoryRepository.findByNameAndStatusTrue(product.getCategory().toUpperCase());
            colorData = colorRepository.findByNameAndStatusTrue(product.getColor().toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (!existsUser) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (existsProduct) {
            throw new BadRequestExceptions(Constants.ErrorProductExists);
        }

        if (modelData == null) {
            throw new BadRequestExceptions(Constants.ErrorModel);
        }

        if (sizeData == null) {
            throw new BadRequestExceptions(Constants.ErrorSize);
        }

        if (categoryData == null) {
            throw new BadRequestExceptions(Constants.ErrorCategory);
        }

        if (colorData == null) {
            throw new BadRequestExceptions(Constants.ErrorColor);
        }

        try {
            productRepository.save(Product.builder()
                    .sku(product.getSku().toUpperCase())
                    .model(modelData)
                    .modelId(modelData.getId())
                    .size(sizeData)
                    .sizeId(sizeData.getId())
                    .category(categoryData)
                    .categoryId(categoryData.getId())
                    .color(colorData)
                    .colorId(colorData.getId())
                    .tokenUser(tokenUser.toUpperCase())
                    .status(true)
                    .registrationDate(new Date(System.currentTimeMillis()))
                    .build());
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e) {
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

    }

    @Override
    public ResponseSuccess saveAll(List<RequestProductSave> products, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {

        boolean existsUser;
        List<Product> productList;
        List<Model> modelList;
        List<Size> sizeList;
        List<Category> categoryList;
        List<Color> colorList;

        try {
            existsUser = userRepository
                    .existsByUsernameAndStatusTrue(tokenUser.toUpperCase());
            productList = productRepository
                    .findBySkuIn(products.stream().map(product -> product.getSku().toUpperCase()).toList());
            modelList = modelRepository
                    .findByNameIn(products.stream().map(product -> product.getModel().toUpperCase()).toList());
            sizeList = sizeRepository
                    .findByNameIn(products.stream().map(product -> product.getSize().toUpperCase()).toList());
            categoryList = categoryRepository
                    .findByNameIn(products.stream().map(product -> product.getCategory().toUpperCase()).toList());
            colorList = colorRepository
                    .findByNameIn(products.stream().map(product -> product.getColor().toUpperCase()).toList());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (!existsUser) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (!productList.isEmpty()) {
            throw new BadRequestExceptions(Constants.ErrorProductExists);
        }

        if (modelList.isEmpty()) {
            throw new BadRequestExceptions(Constants.ErrorModel);
        }

        if (sizeList.isEmpty()) {
            throw new BadRequestExceptions(Constants.ErrorSize);
        }

        if (categoryList.isEmpty()) {
            throw new BadRequestExceptions(Constants.ErrorCategory);
        }

        if (colorList.isEmpty()) {
            throw new BadRequestExceptions(Constants.ErrorColor);
        }

        try {

            List<Product> newProducts = products.stream().map(product -> {

                Model model = modelRepository.findByName(product.getModel().toUpperCase());
                Size size = sizeRepository.findByNameAndStatusTrue(product.getSize().toUpperCase());
                Category category = categoryRepository.findByNameAndStatusTrue(product.getCategory().toUpperCase());
                Color color = colorRepository.findByNameAndStatusTrue(product.getColor().toUpperCase());

                return Product.builder()
                        .sku(product.getSku().toUpperCase())
                        .model(model)
                        .modelId(model.getId())
                        .size(size)
                        .sizeId(size.getId())
                        .category(category)
                        .categoryId(category.getId())
                        .color(color)
                        .colorId(color.getId())
                        .tokenUser(tokenUser.toUpperCase())
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .status(true)
                        .build();
            }).toList();

            productRepository.saveAll(newProducts);

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
    public ResponseDelete delete(String sku, String user) throws InternalErrorExceptions, BadRequestExceptions {

        boolean existsUser;
        Product product;

        try {
            existsUser = userRepository.existsByUsernameAndStatusTrue(user.toUpperCase());
            product = productRepository.findBySku(sku.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (!existsUser) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (product == null) {
            throw new BadRequestExceptions(Constants.ErrorProduct);
        }

        try {
            product.setStatus(false);
            product.setUpdateDate(new Date(System.currentTimeMillis()));
            productRepository.save(product);
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

    }

    @Override
    public Page<ProductDTO> list(String sku, String model, String sort, String sortColumn, Integer pageNumber,
            Integer pageSize) {

        Page<Product> productPage;
        Long modelId;

        try {
            modelId = modelRepository.findByNameAndStatusTrue(model.toUpperCase()).getId();
            productPage = productRepositoryCustom.searchForProduct(sku, modelId, sort, sortColumn, pageNumber,
                    pageSize, true);
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.ResultsFound);
        }

        if (productPage.isEmpty()) {
            return new PageImpl<>(Collections.emptyList());
        }

        List<ProductDTO> productDTOs = productPage.getContent().stream().map(product -> ProductDTO.builder()
                .sku(product.getSku())
                .model(product.getModel().getName())
                .category(product.getCategory().getName())
                .color(product.getColor().getName())
                .size(product.getSize().getName())
                .build()).toList();

        return new PageImpl<>(productDTOs, productPage.getPageable(), productPage.getTotalElements());

    }

}
