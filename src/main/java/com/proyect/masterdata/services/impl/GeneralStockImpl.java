package com.proyect.masterdata.services.impl;

import java.time.OffsetDateTime;
import java.util.*;
import java.util.concurrent.CompletableFuture;

import com.proyect.masterdata.domain.Product;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IUtil;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import com.proyect.masterdata.domain.GeneralStock;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.GeneralStockDTO;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.GeneralStockRepository;
import com.proyect.masterdata.repository.GeneralStockRepositoryCustom;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IGeneralStock;
import com.proyect.masterdata.utils.Constants;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class GeneralStockImpl implements IGeneralStock {

    private final UserRepository userRepository;
    private final GeneralStockRepository generalStockRepository;
    private final GeneralStockRepositoryCustom generalStockRepositoryCustom;
    private final IAudit iAudit;
    private final IUtil iUtil;
    @Override
    public CompletableFuture<ResponseSuccess> in(Product product, Integer quantity, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if (product == null) {
                throw new BadRequestExceptions(Constants.ErrorProduct);
            }

            try {
                GeneralStock generalStock = generalStockRepository.findByClientIdAndProductId(user.getClientId(),
                        product.getId());
                if (generalStock != null) {
                    generalStock.setQuantity(generalStock.getQuantity() + quantity);
                    generalStock.setUpdateDate(OffsetDateTime.now());
                    generalStock.setUser(user);
                    generalStockRepository.save(generalStock);
                } else {
                    generalStockRepository.save(GeneralStock.builder()
                            .quantity(quantity)
                            .client(user.getClient())
                            .clientId(user.getClientId())
                            .registrationDate(OffsetDateTime.now())
                            .product(product)
                            .productId(product.getId())
                            .user(user)
                                    .userId(user.getId())
                            .build());
                }
                String finalSku = iUtil.buildProductSku(product);
                iAudit.save(
                        "ADD_GENERAL_STOCK",
                        "INGRESO DE STOCK "+
                                finalSku+" DE " +
                                quantity +" UNIDADES.",
                        finalSku,user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> out(Product product, Integer quantity, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if (product == null) {
                throw new BadRequestExceptions(Constants.ErrorProduct);
            }

            try {
                GeneralStock generalStock = generalStockRepository.findByClientIdAndProductId(user.getClientId(),
                        product.getId());

                if (generalStock == null) {
                    throw new BadRequestExceptions(Constants.ErrorGeneralStock);
                }

                if (quantity > generalStock.getQuantity()) {
                    throw new BadRequestExceptions(Constants.ErrorGeneralStockLess);
                }

                generalStock.setQuantity(generalStock.getQuantity() - quantity);
                generalStockRepository.save(generalStock);
                String finalSku = iUtil.buildProductSku(product);
                iAudit.save(
                        "DELETE_GENERAL_STOCK",
                        "SALIDA DE STOCK "+
                                finalSku+
                                " DE " + quantity +" UNIDADES.",
                        finalSku,user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<Page<GeneralStockDTO>> list(
            String user,
            String model,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize)
            throws InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<GeneralStock> generalStockPage;
            UUID clientId;

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                generalStockPage = generalStockRepositoryCustom.searchForGeneralStock(
                        clientId,
                        model,
                        registrationStartDate,
                        registrationEndDate,
                        updateStartDate,
                        updateEndDate,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                e.printStackTrace();
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (generalStockPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }

            List<GeneralStockDTO> generalStockDTOs = generalStockPage.getContent().stream()
                    .map(generalStock -> GeneralStockDTO.builder()
                            .id(generalStock.getId())
                            .user(generalStock.getUser().getUsername())
                            .quantity(generalStock.getQuantity())
                            .product(generalStock.getProduct().getName())
                            .productSku(iUtil.buildProductSku(generalStock.getProduct()))
                            .categoryProduct(generalStock.getProduct().getSubCategoryProduct().getCategoryProduct().getName())
                            .subCategoryProduct(generalStock.getProduct().getSubCategoryProduct().getName())
                            .model(generalStock.getProduct().getModel().getName())
                            .color(generalStock.getProduct().getColor().getName())
                            .size(generalStock.getProduct().getSize().getName())
                            .registrationDate(generalStock.getRegistrationDate())
                            .updateDate(generalStock.getUpdateDate())
                            .build())
                    .toList();

            return new PageImpl<>(generalStockDTOs, generalStockPage.getPageable(),
                    generalStockPage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<GeneralStockDTO>> listGeneralStock(String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            UUID clientId;
            List<GeneralStock> generalStocks;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                generalStocks = generalStockRepository.findAllByClientId(clientId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(generalStocks.isEmpty()){
                return Collections.emptyList();
            }

            return generalStocks.stream()
                    .map(generalStock -> GeneralStockDTO.builder()
                            .id(generalStock.getId())
                            .user(generalStock.getUser().getUsername())
                            .quantity(generalStock.getQuantity())
                            .product(generalStock.getProduct().getName())
                            .productSku(iUtil.buildProductSku(generalStock.getProduct()))
                            .categoryProduct(generalStock.getProduct().getSubCategoryProduct().getCategoryProduct().getName())
                            .subCategoryProduct(generalStock.getProduct().getSubCategoryProduct().getName())
                            .model(generalStock.getProduct().getModel().getName())
                            .color(generalStock.getProduct().getColor().getName())
                            .size(generalStock.getProduct().getSize().getName())
                            .registrationDate(generalStock.getRegistrationDate())
                            .updateDate(generalStock.getUpdateDate())
                            .build())
                    .toList();
        });
    }

}
