package com.proyect.masterdata.services.impl;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.OffsetDateTime;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.stream.Stream;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.request.RequestProductUpdate;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.*;
import jakarta.transaction.Transactional;
import org.apache.commons.io.FileUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import com.proyect.masterdata.dto.ProductDTO;
import com.proyect.masterdata.dto.request.RequestProductSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.utils.Constants;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.web.multipart.MultipartFile;

@Service
@RequiredArgsConstructor
@Log4j2
public class ProductImpl implements IProduct {
    private final ProductRepository productRepository;
    private final UserRepository userRepository;
    private final ModelRepository modelRepository;
    private final SizeRepository sizeRepository;
    private final CategoryRepository categoryRepository;
    private final CategoryProductRepository categoryProductRepository;
    private final ColorRepository colorRepository;
    private final ProductRepositoryCustom productRepositoryCustom;
    private final IProductPrice iProductPrice;
    private final UnitRepository unitRepository;
    private final ProductPriceRepository productPriceRepository;
    private final IProductPicture iProductPicture;
    private final ProductPictureRepository productPictureRepository;
    private final IAudit iAudit;
    private final BrandRepository brandRepository;
    private final SubCategoryProductRepository subCategoryProductRepository;
    private final IUtil iUtil;
    @Value("${storage.path.server}")
    private String urlPathServer ;
    @Override
    @Transactional
    public ResponseSuccess save(RequestProductSave requestProductSave,List<MultipartFile> productPictures, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        Product product;
        Model model;
        Size size;
        SubCategoryProduct subCategoryProduct;
        Color color;
        Unit unit;
        Product productName;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            subCategoryProduct = subCategoryProductRepository.findByNameAndStatusTrue(requestProductSave.getSubCategoryProduct().toUpperCase());
            color = colorRepository.findByNameAndStatusTrue(requestProductSave.getColor().toUpperCase());
            unit = unitRepository.findByNameAndUnitTypeIdAndStatusTrue(requestProductSave.getUnit().toUpperCase(),subCategoryProduct.getCategoryProduct().getUnitTypeId());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }else{
            model = modelRepository.findByNameAndClientIdAndStatusTrue(requestProductSave.getModel().toUpperCase(),user.getClientId());
        }

        if (model == null) {
            throw new BadRequestExceptions(Constants.ErrorModel);
        }else{
            size = sizeRepository.findByNameAndStatusTrue(requestProductSave.getSize().toUpperCase());
        }

        if (size == null) {
            throw new BadRequestExceptions(Constants.ErrorSize);
        }

        if (color == null) {
            throw new BadRequestExceptions(Constants.ErrorColor);
        }

        if (subCategoryProduct == null) {
            throw new BadRequestExceptions(Constants.ErrorSubCategory);
        }

        if(!Objects.equals(size.getSizeTypeId(), subCategoryProduct.getCategoryProduct().getSizeTypeId())){
            throw new BadRequestExceptions(Constants.ErrorSizeTypeCategoryProduct);
        }

        if(unit == null){
            throw new BadRequestExceptions(Constants.ErrorUnit);
        }else{
            product = productRepository.findByModelIdAndSizeIdAndColorIdAndClientIdAndStatusTrue(model.getId(),size.getId(),color.getId(),user.getClientId());
        }

        if (product != null) {
            throw new BadRequestExceptions(Constants.ErrorProductExists);
        }else{
            productName = productRepository.findByNameAndClientId(requestProductSave.getName().toUpperCase(),user.getClientId());
        }

        if(productName != null){
            throw new BadRequestExceptions(Constants.ErrorProductExists);
        }

        try {
            Product productData = productRepository.save(Product.builder()
                    .name(requestProductSave.getName().toUpperCase())
                    .model(model)
                    .modelId(model.getId())
                    .size(size)
                    .sizeId(size.getId())
                    .subCategoryProduct(subCategoryProduct)
                    .subCategoryProductId(subCategoryProduct.getId())
                    .color(color)
                    .colorId(color.getId())
                    .unit(unit)
                    .unitId(unit.getId())
                    .client(user.getClient())
                    .clientId(user.getClientId())
                    .user(user)
                    .userId(user.getId())
                    .status(true)
                    .pictureFlag(false)
                    .registrationDate(OffsetDateTime.now())
                    .updateDate(OffsetDateTime.now())
                    .build());
            iProductPrice.save(productData.getId(), requestProductSave.getPrice(),tokenUser.toUpperCase());
            List<String> pictures = iProductPicture.uploadPicture(productPictures,productData.getId(),user.getUsername());
            if(!pictures.isEmpty()){
                productData.setPictureFlag(true);
                productRepository.save(productData);
            }
            iAudit.save("ADD_PRODUCT","PRODUCTO DE MARKETING "+productData.getId()+" CREADO.",productData.getId().toString(),user.getUsername());
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e) {
            e.printStackTrace();
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    @Transactional
    public CompletableFuture<ResponseSuccess> saveAsync(RequestProductSave requestProductSave,MultipartFile[] productPictures, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        Path folder = Paths.get(urlPathServer);
        return CompletableFuture.supplyAsync(()->{
            User user;
            Product product;
            Model model;
            Size size;
            SubCategoryProduct subCategoryProduct;
            Color color;
            Unit unit;
            Product productName;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                subCategoryProduct = subCategoryProductRepository.findByNameAndStatusTrue(requestProductSave.getSubCategoryProduct().toUpperCase());
                color = colorRepository.findByNameAndStatusTrue(requestProductSave.getColor().toUpperCase());
                unit = unitRepository.findByNameAndUnitTypeIdAndStatusTrue(requestProductSave.getUnit().toUpperCase(),subCategoryProduct.getCategoryProduct().getUnitTypeId());
                //subCategoryProduct.getCategoryProduct().getSizeTypeId()
                System.out.println(unitRepository.findAll());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                model = modelRepository.findByNameAndClientIdAndStatusTrue(requestProductSave.getModel().toUpperCase(),user.getClientId());
                //modelRepository.findBySkuAndClientIdAndStatusTrue(requestProductSave.getModel().toUpperCase(),user.getClientId());
            //modelRepository.findByNameAndClientId(requestProductSave.getModel().toUpperCase(),user.getClientId());
                        //
                //System.out.println(modelRepository.findByNameAndClientId(requestProductSave.getModel().toUpperCase(),user.getClientId()));
            }
            //System.out.println(model);

            if (model == null) {
                throw new BadRequestExceptions(Constants.ErrorModel);
            }else{
                size = sizeRepository.findByNameAndStatusTrue(requestProductSave.getSize().toUpperCase());
            }

            if (size == null) {
                throw new BadRequestExceptions(Constants.ErrorSize);
            }

            if (color == null) {
                throw new BadRequestExceptions(Constants.ErrorColor);
            }

            if (subCategoryProduct == null) {
                throw new BadRequestExceptions(Constants.ErrorSubCategory);
            }

            if(!Objects.equals(size.getSizeTypeId(), subCategoryProduct.getCategoryProduct().getSizeTypeId())){
                throw new BadRequestExceptions(Constants.ErrorSizeTypeCategoryProduct);
            }

            if(unit == null){
                throw new BadRequestExceptions(Constants.ErrorUnit);
            }else{
                product = productRepository.findByModelIdAndSizeIdAndColorIdAndClientIdAndStatusTrue(model.getId(),size.getId(),color.getId(),user.getClientId());
            }

            if (product != null) {
                throw new BadRequestExceptions(Constants.ErrorProductExists);
            }else{
                productName = productRepository.findByNameAndClientId(requestProductSave.getName().toUpperCase(),user.getClientId());
            }

            if(productName!=null){
                throw new BadRequestExceptions(Constants.ErrorProductExists);
            }

            try {
                Product productData = productRepository.save(Product.builder()
                        .name(requestProductSave.getName().toUpperCase())
                        .model(model)
                        .modelId(model.getId())
                        .size(size)
                        .sizeId(size.getId())
                        .subCategoryProduct(subCategoryProduct)
                        .subCategoryProductId(subCategoryProduct.getId())
                        .color(color)
                        .colorId(color.getId())
                        .unit(unit)
                        .unitId(unit.getId())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .user(user)
                        .userId(user.getId())
                        .status(true)
                        .updateDate(OffsetDateTime.now())
                        .registrationDate(OffsetDateTime.now())
                        .pictureFlag(false)
                        .build());
                iProductPrice.save(productData.getId(), requestProductSave.getPrice(),tokenUser.toUpperCase());
                List<File> fileList = new ArrayList<>();
                for(MultipartFile multipartFile : productPictures){
                    if(multipartFile.isEmpty()){
                        break;
                    }
                    // /home/powid-masterdata/src/main/resources/uploads/products/

                    File convFile = new File(urlPathServer+multipartFile.getOriginalFilename());
                    convFile.createNewFile();
                    FileOutputStream fos = new FileOutputStream(convFile);
                    fos.write(multipartFile.getBytes());
                    fos.close();
                    fileList.add(convFile);
                }
                CompletableFuture<List<String>> productPhotos = iProductPicture.uploadPictureAsync(fileList,productData.getId(),user.getUsername());
                if(!productPhotos.get().isEmpty()){
                    Stream<Path> paths = Files.list(folder);
                    paths.filter(Files::isRegularFile).forEach(path -> {
                        try {
                            Files.delete(path);
                        } catch (IOException e) {
                            throw new RuntimeException(e);
                        }
                    });
                    productData.setPictureFlag(true);
                    productRepository.save(productData);
                }
                iAudit.save("ADD_PRODUCT","PRODUCTO DE MARKETING "+productData.getId()+" CREADO.",productData.getId().toString(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            } catch (RuntimeException | IOException | InterruptedException | ExecutionException e) {
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseDelete> delete(UUID productId, String username) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Product product;

            try {
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                product = productRepository.findByIdAndStatusTrue(productId);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user==null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if (product == null) {
                throw new BadRequestExceptions(Constants.ErrorProduct);
            }

            try {
                product.setStatus(false);
                product.setUpdateDate(OffsetDateTime.now());
                product.setUser(user);
                product.setUserId(user.getId());
                productRepository.save(product);
                String finalSku = iUtil.buildProductSku(product);
                iAudit.save("DELETE_PRODUCT","PRODUCTO DE MARKETING "+finalSku+" DESACTIVADO.",finalSku,user.getUsername());
                return ResponseDelete.builder()
                        .code(200)
                        .message(Constants.delete)
                        .build();
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> activate(UUID productId, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Product product;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                product = productRepository.findByIdAndStatusFalse(productId);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user==null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if (product == null) {
                throw new BadRequestExceptions(Constants.ErrorProduct);
            }

            try {
                product.setStatus(true);
                product.setUpdateDate(OffsetDateTime.now());
                product.setUser(user);
                product.setUserId(user.getId());
                productRepository.save(product);
                String finalSku = iUtil.buildProductSku(product);
                iAudit.save("ACTIVATE_PRODUCT","PRODUCTO DE MARKETING "+finalSku+" ACTIVADO.",finalSku,user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.update)
                        .build();
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<Page<ProductDTO>> list(
            String tokenUser,
            String sku,
            String model,
            List<String> brands,
            List<String> sizes,
            List<String> categoryProducts,
            List<String> colors,
            List<String> units,
            Boolean pictureFlag,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) {
        return CompletableFuture.supplyAsync(()->{
            Page<Product> productPage;
            List<UUID> brandIds;
            List<UUID> sizeIds;
            List<UUID> categoryProductIds;
            List<UUID> colorIds;
            List<UUID> unitIds;
            UUID clientId;

            if(sizes != null && !sizes.isEmpty()){
                sizeIds = sizeRepository.findByNameIn(
                        sizes.stream().map(String::toUpperCase).toList()
                ).stream().map(Size::getId).toList();
            }else{
                sizeIds = new ArrayList<>();
            }

            if(categoryProducts != null && !categoryProducts.isEmpty()){
                categoryProductIds = categoryProductRepository.findByNameIn(
                        categoryProducts.stream().map(String::toUpperCase).toList()
                ).stream().map(CategoryProduct::getId).toList();
            }else{
                categoryProductIds = new ArrayList<>();
            }

            if(colors != null && !colors.isEmpty()){
                colorIds = colorRepository.findByNameIn(
                        colors.stream().map(String::toUpperCase).toList()
                ).stream().map(Color::getId).toList();
            }else{
                colorIds = new ArrayList<>();
            }

            if(units != null && !units.isEmpty()){
                unitIds = unitRepository.findByNameIn(
                        units.stream().map(String::toUpperCase).toList()
                ).stream().map(Unit::getId).toList();
            }else {
                unitIds = new ArrayList<>();
            }

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase()).getClient().getId();
                if(brands != null && !brands.isEmpty()){
                    brandIds = brandRepository.findByClientIdAndNameIn(
                            clientId,
                            brands.stream().map(String::toUpperCase).toList()
                    ).stream().map(Brand::getId).toList();
                }else{
                    brandIds = new ArrayList<>();
                }
                productPage = productRepositoryCustom.searchForProduct(
                        clientId,
                        sku,
                        model,
                        brandIds,
                        sizeIds,
                        categoryProductIds,
                        colorIds,
                        unitIds,
                        pictureFlag,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        true);

            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (productPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }

            List<ProductDTO> productDTOs = productPage.getContent().stream().map(product -> {
                ProductPrice productPrice = productPriceRepository.findByProductIdAndStatusTrue(product.getId());
                List<String> productImages = productPictureRepository.findAllByProductId(product.getId()).stream().map(ProductPicture::getProductPictureUrl).toList();

                return ProductDTO.builder()
                        .id(product.getId())
                        .name(product.getName().toUpperCase())
                        .sku(iUtil.buildProductSku(product))
                        .brand(product.getModel().getBrand().getName())
                        .model(product.getModel().getName())
                        .category(product.getSubCategoryProduct().getCategoryProduct().getName())
                        .subCategory(product.getSubCategoryProduct().getName())
                        .color(product.getColor().getName())
                        .size(product.getSize().getName())
                        .unit(product.getUnit().getName())
                        .price(productPrice.getUnitSalePrice())
                        .pictures(productImages)
                        .registrationDate(product.getRegistrationDate())
                        .updateDate(product.getUpdateDate())
                        .pictureFlag(product.getPictureFlag())
                        .build();
            }).toList();

            return new PageImpl<>(productDTOs, productPage.getPageable(), productPage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<Page<ProductDTO>> listFalse(
            String tokenUser,
            String sku,
            String model,
            List<String> brands,
            List<String> sizes,
            List<String> categoryProducts,
            List<String> colors,
            List<String> units,
            Boolean pictureFlag,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<Product> productPage;
            List<UUID> brandIds;
            List<UUID> sizeIds;
            List<UUID> categoryProductIds;
            List<UUID> colorIds;
            List<UUID> unitIds;
            UUID clientId;

            if(sizes != null && !sizes.isEmpty()){
                sizeIds = sizeRepository.findByNameIn(
                        sizes.stream().map(String::toUpperCase).toList()
                ).stream().map(Size::getId).toList();
            }else{
                sizeIds = new ArrayList<>();
            }

            if(categoryProducts != null && !categoryProducts.isEmpty()){
                categoryProductIds = categoryProductRepository.findByNameIn(
                        categoryProducts.stream().map(String::toUpperCase).toList()
                ).stream().map(CategoryProduct::getId).toList();
            }else{
                categoryProductIds = new ArrayList<>();
            }

            if(colors != null && !colors.isEmpty()){
                colorIds = colorRepository.findByNameIn(
                        colors.stream().map(String::toUpperCase).toList()
                ).stream().map(Color::getId).toList();
            }else{
                colorIds = new ArrayList<>();
            }

            if(units != null && !units.isEmpty()){
                unitIds = unitRepository.findByNameIn(
                        units.stream().map(String::toUpperCase).toList()
                ).stream().map(Unit::getId).toList();
            }else {
                unitIds = new ArrayList<>();
            }

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase()).getClient().getId();
                if(brands != null && !brands.isEmpty()){
                    brandIds = brandRepository.findByClientIdAndNameIn(
                            clientId,
                            brands.stream().map(String::toUpperCase).toList()
                    ).stream().map(Brand::getId).toList();
                }else{
                    brandIds = new ArrayList<>();
                }
                productPage = productRepositoryCustom.searchForProduct(
                        clientId,
                        sku,
                        model,
                        brandIds,
                        sizeIds,
                        categoryProductIds,
                        colorIds,
                        unitIds,
                        pictureFlag,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        false);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (productPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }

            List<ProductDTO> productDTOs = productPage.getContent().stream().map(product -> {
                ProductPrice productPrice = productPriceRepository.findByProductIdAndStatusTrue(product.getId());
                return ProductDTO.builder()
                        .id(product.getId())
                        .name(product.getName().toUpperCase())
                        .sku(iUtil.buildProductSku(product))
                        .brand(product.getModel().getBrand().getName())
                        .model(product.getModel().getName())
                        .category(product.getSubCategoryProduct().getCategoryProduct().getName())
                        .subCategory(product.getSubCategoryProduct().getName())
                        .color(product.getColor().getName())
                        .size(product.getSize().getName())
                        .unit(product.getUnit().getName())
                        .price(productPrice.getUnitSalePrice())
                        .pictureFlag(product.getPictureFlag())
                        .registrationDate(product.getRegistrationDate())
                        .updateDate(product.getUpdateDate())
                        .build();
            }).toList();

            return new PageImpl<>(productDTOs, productPage.getPageable(), productPage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<ProductDTO>> listProducts(String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<Product> products;
            UUID clientId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                products = productRepository.findAllByClientIdAndStatusTrue(clientId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(products.isEmpty()){
                return Collections.emptyList();
            }

            return products.stream().map(product -> {
                ProductPrice productPrice = productPriceRepository.findByProductIdAndStatusTrue(product.getId());
                return ProductDTO.builder()
                        .id(product.getId())
                        .name(product.getName().toUpperCase())
                        .sku(iUtil.buildProductSku(product))
                        .brand(product.getModel().getBrand().getName())
                        .model(product.getModel().getName())
                        .category(product.getSubCategoryProduct().getCategoryProduct().getName())
                        .subCategory(product.getSubCategoryProduct().getName())
                        .color(product.getColor().getName())
                        .size(product.getSize().getName())
                        .unit(product.getUnit().getName())
                        .price(productPrice.getUnitSalePrice())
                        .registrationDate(product.getRegistrationDate())
                        .updateDate(product.getUpdateDate())
                        .build();
            }).toList();
        });
    }

    @Override
    public CompletableFuture<List<ProductDTO>> listProductsFalse(String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<Product> products;
            UUID clientId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                products = productRepository.findAllByClientIdAndStatusFalse(clientId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(products.isEmpty()){
                return Collections.emptyList();
            }

            return products.stream().map(product -> {
                ProductPrice productPrice = productPriceRepository.findByProductIdAndStatusTrue(product.getId());
                return ProductDTO.builder()
                        .id(product.getId())
                        .name(product.getName().toUpperCase())
                        .sku(iUtil.buildProductSku(product))
                        .brand(product.getModel().getBrand().getName())
                        .model(product.getModel().getName())
                        .category(product.getSubCategoryProduct().getCategoryProduct().getName())
                        .subCategory(product.getSubCategoryProduct().getName())
                        .color(product.getColor().getName())
                        .size(product.getSize().getName())
                        .unit(product.getUnit().getName())
                        .price(productPrice.getUnitSalePrice())
                        .registrationDate(product.getRegistrationDate())
                        .updateDate(product.getUpdateDate())
                        .build();
            }).toList();
        });
    }

    @Override
    public CompletableFuture<List<ProductDTO>> listFilter(String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<Product> products;
            UUID clientId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                products = productRepository.findAllByClientId(clientId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(products.isEmpty()){
                return Collections.emptyList();
            }

            return products.stream().map(product -> {
                ProductPrice productPrice = productPriceRepository.findByProductIdAndStatusTrue(product.getId());
                return ProductDTO.builder()
                        .id(product.getId())
                        .name(product.getName().toUpperCase())
                        .sku(iUtil.buildProductSku(product))
                        .brand(product.getModel().getBrand().getName())
                        .model(product.getModel().getName())
                        .category(product.getSubCategoryProduct().getCategoryProduct().getName())
                        .subCategory(product.getSubCategoryProduct().getName())
                        .color(product.getColor().getName())
                        .size(product.getSize().getName())
                        .unit(product.getUnit().getName())
                        .price(productPrice.getUnitSalePrice())
                        .registrationDate(product.getRegistrationDate())
                        .updateDate(product.getUpdateDate())
                        .build();
            }).toList();
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> update(RequestProductUpdate requestProductUpdate, List<MultipartFile> pictures) throws BadRequestExceptions, InternalErrorExceptions {
        Path folder = Paths.get(urlPathServer);

        return CompletableFuture.supplyAsync(()->{
            User user;
            Product product;
            try {
                user = userRepository.findByUsernameAndStatusTrue(requestProductUpdate.getTokenUser().toUpperCase());
                product = productRepository.findByIdAndStatusTrue(requestProductUpdate.getProductId());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(product==null){
                throw new BadRequestExceptions(Constants.ErrorProduct);
            }

            try {
                if(requestProductUpdate.getPrice()!=null){
                    ProductPrice productPrice = productPriceRepository.findByProductIdAndStatusTrue(product.getId());
                    productPrice.setStatus(false);
                    productPriceRepository.save(productPrice);
                    productPriceRepository.save(ProductPrice.builder()
                                    .unitSalePrice(requestProductUpdate.getPrice())
                                    .product(product)
                                    .productId(product.getId())
                                    .registrationDate(OffsetDateTime.now())
                                    .updateDate(OffsetDateTime.now())
                                    .user(user)
                                    .userId(user.getId())
                                    .status(true)
                            .build());
                }
                List<File> fileList = new ArrayList<>();
                for(MultipartFile multipartFile : pictures){
                    if(multipartFile.isEmpty()){
                        break;
                    }
                    File convFile = new File(urlPathServer+multipartFile.getOriginalFilename());
                    convFile.createNewFile();
                    FileOutputStream fos = new FileOutputStream(convFile);
                    fos.write(multipartFile.getBytes());
                    fos.close();
                    fileList.add(convFile);
                }
                CompletableFuture<List<String>> productPhotos = iProductPicture.uploadPictureAsync(fileList,product.getId(),user.getUsername());
                if(!productPhotos.get().isEmpty()){
                    Stream<Path> paths = Files.list(folder);
                    paths.filter(Files::isRegularFile).forEach(path -> {
                        try {
                            Files.delete(path);
                        } catch (IOException e) {
                            throw new RuntimeException(e);
                        }
                    });
                    if(!product.getPictureFlag()){
                        product.setPictureFlag(true);
                        productRepository.save(product);
                    }
                }
                String finalSku = iUtil.buildProductSku(product);
                iAudit.save("UPDATE_PRODUCT","PRODUCTO DE MARKETING "+finalSku+" ACTUALIZADO.",finalSku,user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            }catch (RuntimeException | IOException | InterruptedException | ExecutionException e){
                e.printStackTrace();
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<List<ProductDTO>> listByColorAndSize(String color, String size,String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<Product> products;
            UUID clientId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(username.toUpperCase()).getClientId();
                products = productRepository.findByColorNameAndSizeNameAndClientIdAndStatusTrue(
                        color.toUpperCase(),
                        size.toUpperCase(),
                        clientId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(products.isEmpty()){
                return Collections.emptyList();
            }

            return products.stream().map(product -> {
                ProductPrice productPrice = productPriceRepository.findByProductIdAndStatusTrue(product.getId());
                return ProductDTO.builder()
                        .id(product.getId())
                        .name(product.getName().toUpperCase())
                        .sku(iUtil.buildProductSku(product))
                        .brand(product.getModel().getBrand().getName())
                        .model(product.getModel().getName())
                        .category(product.getSubCategoryProduct().getCategoryProduct().getName())
                        .subCategory(product.getSubCategoryProduct().getName())
                        .color(product.getColor().getName())
                        .size(product.getSize().getName())
                        .unit(product.getUnit().getName())
                        .price(productPrice.getUnitSalePrice())
                        .registrationDate(product.getRegistrationDate())
                        .updateDate(product.getUpdateDate())
                        .build();
            }).toList();
        });
    }

    @Override
    public CompletableFuture<List<ProductDTO>> listByModelAndSizeAndColor(String model, String size, String color,String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<Product> products;
            UUID clientId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(username.toUpperCase()).getClientId();
                products = productRepository.findByModelNameAndSizeNameAndColorNameAndClientIdAndStatusTrue(
                        model.toUpperCase(),
                        size.toUpperCase(),
                        color.toUpperCase(),
                        clientId
                );
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(products.isEmpty()){
                return Collections.emptyList();
            }

            return products.stream().map(product -> {
                ProductPrice productPrice = productPriceRepository.findByProductIdAndStatusTrue(product.getId());
                return ProductDTO.builder()
                        .id(product.getId())
                        .name(product.getName().toUpperCase())
                        .sku(iUtil.buildProductSku(product))
                        .brand(product.getModel().getBrand().getName())
                        .model(product.getModel().getName())
                        .category(product.getSubCategoryProduct().getCategoryProduct().getName())
                        .subCategory(product.getSubCategoryProduct().getName())
                        .color(product.getColor().getName())
                        .size(product.getSize().getName())
                        .unit(product.getUnit().getName())
                        .price(productPrice.getUnitSalePrice())
                        .registrationDate(product.getRegistrationDate())
                        .updateDate(product.getUpdateDate())
                        .build();
            }).toList();
        });
    }

    @Override
    public CompletableFuture<List<ProductDTO>> listByModelAndColor(String model, String color,String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<Product> products;
            UUID clientId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                products = productRepository.findByModelNameAndColorNameAndClientIdAndStatusTrue(
                        model.toUpperCase(),
                        color.toUpperCase(),
                        clientId
                );
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(products.isEmpty()){
                return Collections.emptyList();
            }

            return products.stream().map(product -> {
                ProductPrice productPrice = productPriceRepository.findByProductIdAndStatusTrue(product.getId());
                return ProductDTO.builder()
                        .id(product.getId())
                        .name(product.getName().toUpperCase())
                        .sku(iUtil.buildProductSku(product))
                        .brand(product.getModel().getBrand().getName())
                        .model(product.getModel().getName())
                        .category(product.getSubCategoryProduct().getCategoryProduct().getName())
                        .subCategory(product.getSubCategoryProduct().getName())
                        .color(product.getColor().getName())
                        .size(product.getSize().getName())
                        .unit(product.getUnit().getName())
                        .price(productPrice.getUnitSalePrice())
                        .registrationDate(product.getRegistrationDate())
                        .updateDate(product.getUpdateDate())
                        .build();
            }).toList();
        });
    }

    @Override
    public CompletableFuture<List<ProductDTO>> listByModel(String modelSku, String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<Product> products;
            UUID clientId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                products = productRepository.findByModelNameAndClientIdAndStatusTrue(
                        modelSku.toUpperCase(),
                        clientId
                );
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(products.isEmpty()){
                return Collections.emptyList();
            }

            return products.stream().map(product -> {
                ProductPrice productPrice = productPriceRepository.findByProductIdAndStatusTrue(product.getId());
                return ProductDTO.builder()
                        .id(product.getId())
                        .name(product.getName().toUpperCase())
                        .sku(iUtil.buildProductSku(product))
                        .brand(product.getModel().getBrand().getName())
                        .model(product.getModel().getName())
                        .category(product.getSubCategoryProduct().getCategoryProduct().getName())
                        .subCategory(product.getSubCategoryProduct().getName())
                        .color(product.getColor().getName())
                        .size(product.getSize().getName())
                        .unit(product.getUnit().getName())
                        .price(productPrice.getUnitSalePrice())
                        .registrationDate(product.getRegistrationDate())
                        .updateDate(product.getUpdateDate())
                        .build();
            }).toList();
        });
    }
}
