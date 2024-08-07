package com.proyect.masterdata.services;

import java.util.List;
import java.util.concurrent.CompletableFuture;

import com.proyect.masterdata.domain.Model;
import org.springframework.data.domain.Page;

import com.proyect.masterdata.dto.ProductDTO;
import com.proyect.masterdata.dto.request.RequestProductSave;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.web.multipart.MultipartFile;

public interface IProduct {
        ResponseSuccess save(RequestProductSave product, List<MultipartFile> productPictures, String tokenUser)
                        throws InternalErrorExceptions, BadRequestExceptions;
        CompletableFuture<ResponseSuccess> saveAsync(RequestProductSave product, MultipartFile[] productPictures, String tokenUser)
                throws InternalErrorExceptions, BadRequestExceptions;
        CompletableFuture<ResponseDelete> delete(String sku, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
        CompletableFuture<ResponseSuccess> activate(String sku, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
        CompletableFuture<Page<ProductDTO>> list(
                String tokenUser,
                List<String> skus,
                List<String> models,
                List<String> brands,
                List<String> sizes,
                List<String> categoryProducts,
                List<String> colors,
                List<String> units,
                String sort,
                String sortColumn,
                Integer pageNumber,
                Integer pageSize) throws BadRequestExceptions;
        CompletableFuture<Page<ProductDTO>> listFalse(
                String tokenUser,
                List<String> skus,
                List<String> models,
                List<String> brands,
                List<String> sizes,
                List<String> categoryProducts,
                List<String> colors,
                List<String> units,
                String sort,
                String sortColumn,
                Integer pageNumber,
                Integer pageSize) throws BadRequestExceptions;
        CompletableFuture<List<ProductDTO>> listProducts(String user) throws BadRequestExceptions,InternalErrorExceptions;
        CompletableFuture<List<ProductDTO>> listProductsFalse(String user) throws BadRequestExceptions,InternalErrorExceptions;
        CompletableFuture<List<ProductDTO>> listProductsModel(String user,String model) throws BadRequestExceptions,InternalErrorExceptions;
        CompletableFuture<List<ProductDTO>> listFilter(String user) throws BadRequestExceptions,InternalErrorExceptions;
}
