package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.PurchasePaymentMethod;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.PurchasePaymentMethodDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.mapper.PaymentMethodMapper;
import com.proyect.masterdata.repository.PurchasePaymentMethodRepository;
import com.proyect.masterdata.repository.PurchasePaymentMethodRepositoryCustom;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IPurchasePaymentMethod;
import com.proyect.masterdata.utils.Constants;
import lombok.AllArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@AllArgsConstructor
@Log4j2
public class PurchasePaymentMethodImpl implements IPurchasePaymentMethod {
    private final PurchasePaymentMethodRepository purchasePaymentMethodRepository;
    private final UserRepository userRepository;
    private final PurchasePaymentMethodRepositoryCustom purchasePaymentMethodRepositoryCustom;
    private final IAudit iAudit;
    @Override
    public CompletableFuture<ResponseSuccess> save(String name, String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            PurchasePaymentMethod purchasePaymentMethod;

            try {
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                purchasePaymentMethod = purchasePaymentMethodRepository.findByNameAndStatusTrue(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }
            if (purchasePaymentMethod != null) {
                throw new BadRequestExceptions(Constants.ErrorPaymentMethodExists.toUpperCase());
            }

            try {
                PurchasePaymentMethod newPurchasePaymentMethod = purchasePaymentMethodRepository.save(PurchasePaymentMethod.builder()
                        .user(user)
                        .userId(user.getId())
                        .status(true)
                        .name(name.toUpperCase())
                        .registrationDate(OffsetDateTime.now())
                        .build());
                iAudit.save("ADD_ORDER_PAYMENT_METHOD","METODO DE PAGO "+newPurchasePaymentMethod.getName()+" CREADO.",newPurchasePaymentMethod.getName(),user.getUsername());
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
    @Transactional
    public CompletableFuture<ResponseDelete> delete(String name, String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            PurchasePaymentMethod purchasePaymentMethod;

            try {
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                purchasePaymentMethod = purchasePaymentMethodRepository.findByNameAndStatusTrue(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e);
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }
            if (purchasePaymentMethod == null) {
                throw new BadRequestExceptions(Constants.ErrorPaymentMethod.toUpperCase());
            }

            try {
                purchasePaymentMethod.setStatus(false);
                purchasePaymentMethod.setUpdateDate(OffsetDateTime.now());
                purchasePaymentMethod.setUser(user);
                purchasePaymentMethod.setUserId(user.getId());
                purchasePaymentMethodRepository.save(purchasePaymentMethod);
                iAudit.save("DELETE_ORDER_PAYMENT_METHOD","METODO DE PAGO "+purchasePaymentMethod.getName()+" DESACTIVADO.",purchasePaymentMethod.getName(),user.getUsername());
                return ResponseDelete.builder()
                        .code(200)
                        .message(Constants.delete)
                        .build();
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> activate(String name, String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            PurchasePaymentMethod purchasePaymentMethod;

            try {
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                purchasePaymentMethod = purchasePaymentMethodRepository.findByNameAndStatusTrue(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e);
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }
            if (purchasePaymentMethod == null) {
                throw new BadRequestExceptions(Constants.ErrorPaymentMethod.toUpperCase());
            }

            try {
                purchasePaymentMethod.setStatus(true);
                purchasePaymentMethod.setUpdateDate(OffsetDateTime.now());
                purchasePaymentMethod.setUser(user);
                purchasePaymentMethod.setUserId(user.getId());
                purchasePaymentMethodRepository.save(purchasePaymentMethod);
                iAudit.save("ACTIVATE_ORDER_PAYMENT_METHOD","METODO DE PAGO "+purchasePaymentMethod.getName()+" ACTIVADO.",purchasePaymentMethod.getName(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.update)
                        .build();
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<List<PurchasePaymentMethodDTO>> listPaymentMethod() throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<PurchasePaymentMethod> purchasePaymentMethods = new ArrayList<>();
            try {
                purchasePaymentMethods = purchasePaymentMethodRepository.findAllByStatusTrue();
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if (purchasePaymentMethods.isEmpty()) {
                return Collections.emptyList();
            }
            return purchasePaymentMethods.stream().map(purchasePaymentMethod -> PurchasePaymentMethodDTO.builder()
                    .id(purchasePaymentMethod.getId())
                    .name(purchasePaymentMethod.getName())
                    .user(purchasePaymentMethod.getUser().getUsername())
                    .status(purchasePaymentMethod.getStatus())
                    .build()).toList();
        });
    }

    @Override
    public CompletableFuture<Page<PurchasePaymentMethodDTO>> list(
            String name,
            String user,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<PurchasePaymentMethod> purchasePaymentMethodPage;
            try {
                purchasePaymentMethodPage = purchasePaymentMethodRepositoryCustom.searchForPurchasePaymentMethod(
                        name,
                        user,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        status);
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (purchasePaymentMethodPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }

            List<PurchasePaymentMethodDTO> purchasePaymentMethodDTOS = purchasePaymentMethodPage.getContent().stream().map(purchasePaymentMethod -> PurchasePaymentMethodDTO.builder()
                    .id(purchasePaymentMethod.getId())
                    .name(purchasePaymentMethod.getName())
                    .user(purchasePaymentMethod.getUser().getUsername())
                    .status(purchasePaymentMethod.getStatus())
                    .build()).toList();

            return new PageImpl<>(
                    purchasePaymentMethodDTOS,
                    purchasePaymentMethodPage.getPageable(), purchasePaymentMethodPage.getTotalElements());
        });
    }
    @Override
    public CompletableFuture<List<PurchasePaymentMethodDTO>> listFilter() throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<PurchasePaymentMethod> purchasePaymentMethods = new ArrayList<>();
            try {
                purchasePaymentMethods = purchasePaymentMethodRepository.findAll();
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if (purchasePaymentMethods.isEmpty()) {
                return Collections.emptyList();
            }
            return purchasePaymentMethods.stream().map(purchasePaymentMethod -> PurchasePaymentMethodDTO.builder()
                    .id(purchasePaymentMethod.getId())
                    .name(purchasePaymentMethod.getName())
                    .user(purchasePaymentMethod.getUser().getUsername())
                    .status(purchasePaymentMethod.getStatus())
                    .build()).toList();
        });
    }
}
