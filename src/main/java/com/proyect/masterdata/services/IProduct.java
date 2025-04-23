package com.proyect.masterdata.services;

import java.time.OffsetDateTime;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

import com.proyect.masterdata.domain.Model;
import com.proyect.masterdata.dto.request.RequestProductUpdate;
import org.springframework.data.domain.Page;

import com.proyect.masterdata.dto.ProductDTO;
import com.proyect.masterdata.dto.request.RequestProductSave;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Pageable;
import org.springframework.web.multipart.MultipartFile;

public interface IProduct {
        ResponseSuccess save(RequestProductSave product, List<MultipartFile> productPictures, String tokenUser)
                        throws InternalErrorExceptions, BadRequestExceptions;
        CompletableFuture<ResponseSuccess> saveAsync(RequestProductSave product, MultipartFile[] productPictures, String tokenUser)
                throws InternalErrorExceptions, BadRequestExceptions;
        CompletableFuture<ResponseDelete> delete(UUID productId, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
        CompletableFuture<ResponseSuccess> activate(UUID productId, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
        CompletableFuture<Page<ProductDTO>> list(
                String tokenUser,
                String productSku,
                String product,
                String model,
                String brand,
                String size,
                String categoryProduct,
                String subCategoryProduct,
                String color,
                String unit,
                Boolean pictureFlag,
                OffsetDateTime registrationStartDate,
                OffsetDateTime registrationEndDate,
                OffsetDateTime updateStartDate,
                OffsetDateTime updateEndDate,
                String sort,
                String sortColumn,
                Integer pageNumber,
                Integer pageSize,
                Boolean status) throws BadRequestExceptions;
        CompletableFuture<List<ProductDTO>> listProducts(String user) throws BadRequestExceptions,InternalErrorExceptions;
        CompletableFuture<List<ProductDTO>> listProductsFalse(String user) throws BadRequestExceptions,InternalErrorExceptions;
        CompletableFuture<List<ProductDTO>> listFilter(String user) throws BadRequestExceptions,InternalErrorExceptions;
        CompletableFuture<ResponseSuccess> update(RequestProductUpdate requestProductUpdate, List<MultipartFile> pictures) throws BadRequestExceptions,InternalErrorExceptions;
        CompletableFuture<List<ProductDTO>> listByColorAndSize(String color,String size,String user) throws BadRequestExceptions,InternalErrorExceptions;
        CompletableFuture<List<ProductDTO>> listByModelAndSizeAndColor(String model,String size,String color,String user) throws BadRequestExceptions,InternalErrorExceptions;
        CompletableFuture<List<ProductDTO>> listByModelAndColor(String model,String color,String user) throws BadRequestExceptions,InternalErrorExceptions;
        CompletableFuture<List<ProductDTO>> listByModel(String modelSku,String user) throws BadRequestExceptions,InternalErrorExceptions;
        public Page<ProductDTO> searchProducts(String userName, String nameQuery, Pageable pageable) throws BadRequestExceptions,InternalErrorExceptions;

}
