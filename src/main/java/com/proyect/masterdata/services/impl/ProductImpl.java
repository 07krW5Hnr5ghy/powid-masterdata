package com.proyect.masterdata.services.impl;

import java.util.Date;
import java.util.List;

import org.springframework.stereotype.Service;

import com.proyect.masterdata.domain.Category;
import com.proyect.masterdata.domain.Color;
import com.proyect.masterdata.domain.Model;
import com.proyect.masterdata.domain.Product;
import com.proyect.masterdata.domain.Size;
import com.proyect.masterdata.dto.request.RequestProductSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
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
    public ResponseSuccess save(RequestProductSave product, String user)
            throws InternalErrorExceptions, BadRequestExceptions {

        boolean existsUser;
        boolean existsProduct;
        Model modelData;
        Size sizeData;
        Category categoryData;
        Color colorData;

        try {
            existsUser = userRepository.existsByUser(user.toUpperCase());
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
                    .sku(product.getSku().toUpperCase())
                    .model(modelData)
                    .idModel(modelData.getId())
                    .size(sizeData)
                    .idSize(sizeData.getId())
                    .category(categoryData)
                    .idCategory(categoryData.getId())
                    .color(colorData)
                    .idColor(colorData.getId())
                    .user(user.toUpperCase())
                    .status(true)
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

    @Override
    public ResponseSuccess saveAll(List<RequestProductSave> products, String user)
            throws InternalErrorExceptions, BadRequestExceptions {

        boolean existsUser;
        List<Product> productList;
        List<Model> modelList;
        List<Size> sizeList;
        List<Category> categoryList;
        List<Color> colorList;

        try {
            existsUser = userRepository
                    .existsByUser(user.toUpperCase());
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
            throw new BadRequestExceptions("Usuario no existe");
        }

        if (!productList.isEmpty()) {
            throw new BadRequestExceptions("Producto ya existe");
        }

        if (modelList.size() != products.size()) {
            throw new BadRequestExceptions("Modelo no existe");
        }

        if (sizeList.size() != products.size()) {
            throw new BadRequestExceptions("Talla no existe");
        }

        if (categoryList.size() != products.size()) {
            throw new BadRequestExceptions("Categoria no existe");
        }

        if (colorList.size() != products.size()) {
            throw new BadRequestExceptions("Color no existe");
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
                        .idModel(model.getId())
                        .size(size)
                        .idSize(size.getId())
                        .category(category)
                        .idCategory(category.getId())
                        .color(color)
                        .idColor(color.getId())
                        .user(user.toUpperCase())
                        .dateRegistration(new Date(System.currentTimeMillis()))
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
            existsUser = userRepository.existsByUser(user.toUpperCase());
            product = productRepository.findBySku(sku.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (!existsUser) {
            throw new BadRequestExceptions("Usuario no existe");
        }

        if (product == null) {
            throw new BadRequestExceptions("Producto no existe");
        }

        try {
            product.setStatus(false);
            product.setDateUpdate(new Date(System.currentTimeMillis()));
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

}
