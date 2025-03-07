package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.services.IAudit;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import com.proyect.masterdata.domain.Subscription;
import com.proyect.masterdata.domain.Module;
import com.proyect.masterdata.dto.ModulePlanDTO;
import com.proyect.masterdata.dto.PlanDTO;
import com.proyect.masterdata.dto.SubscriptionDTO;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.ModuleRepository;
import com.proyect.masterdata.repository.SubscriptionRepository;
import com.proyect.masterdata.repository.SubscriptionRepositoryCustom;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.ISubscription;
import com.proyect.masterdata.utils.Constants;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.OffsetDateTime;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class SubscriptionImpl implements ISubscription {
    private final SubscriptionRepository subscriptionRepository;
    private final UserRepository userRepository;
    private final SubscriptionRepositoryCustom subscriptionRepositoryCustom;
    private final ModuleRepository moduleRepository;
    private final IAudit iAudit;
    @Override
    public ResponseSuccess save(String name, Integer months, Double discountPercent, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        boolean existsSubscription;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            existsSubscription = subscriptionRepository.existsByNameAndStatusTrue(name.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user==null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (existsSubscription) {
            throw new BadRequestExceptions(Constants.ErrorSubscriptionExists);
        }

        try {
            Subscription newSubscription = subscriptionRepository.save(Subscription.builder()
                    .name(name.toUpperCase())
                    .months(months)
                    .discountPercent(discountPercent)
                    .registrationDate(OffsetDateTime.now())
                    .user(user).userId(user.getId())
                    .status(true)
                    .build());
            iAudit.save("ADD_SUBSCRIPTION","SUBSCRIPCION "+newSubscription.getName()+" CREADO.",newSubscription.getName(),user.getUsername());
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
    public CompletableFuture<ResponseSuccess> saveAsync(String name, Integer months, Double discountPercent, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            boolean existsSubscription;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                existsSubscription = subscriptionRepository.existsByNameAndStatusTrue(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user==null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if (existsSubscription) {
                throw new BadRequestExceptions(Constants.ErrorSubscriptionExists);
            }

            try {
                Subscription newSubscription = subscriptionRepository.save(Subscription.builder()
                        .name(name.toUpperCase())
                        .months(months)
                        .discountPercent(discountPercent)
                        .registrationDate(OffsetDateTime.now())
                        .user(user).userId(user.getId())
                        .status(true)
                        .build());
                iAudit.save("ADD_SUBSCRIPTION","SUBSCRIPCION "+newSubscription.getName()+" CREADO.",newSubscription.getName(),user.getUsername());
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
    public CompletableFuture<ResponseDelete> delete(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Subscription subscription;
            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                subscription = subscriptionRepository.findByNameAndStatusTrue(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(subscription==null){
                throw new BadRequestExceptions(Constants.ErrorSubscription);
            }
            try {
                subscription.setStatus(false);
                subscription.setUpdateDate(OffsetDateTime.now());
                subscription.setUser(user);
                subscription.setUserId(user.getId());
                iAudit.save("DELETE_SUBSCRIPTION","SUBSCRIPCION "+subscription.getName()+" DESACTIVADO.",subscription.getName(),user.getUsername());
                return ResponseDelete.builder()
                        .code(200)
                        .message(Constants.delete)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> activate(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Subscription subscription;
            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                subscription = subscriptionRepository.findByNameAndStatusFalse(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(subscription==null){
                throw new BadRequestExceptions(Constants.ErrorSubscription);
            }
            try {
                subscription.setStatus(true);
                subscription.setUpdateDate(OffsetDateTime.now());
                subscription.setUser(user);
                subscription.setUserId(user.getId());
                iAudit.save("ACTIVATE_SUBSCRIPTION","SUBSCRIPCION "+subscription.getName()+" ACTIVADA.",subscription.getName(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.update)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<Page<SubscriptionDTO>> list(String name, String user, String sort, String sortColumn, Integer pageNumber,
            Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<Subscription> subscriptionPage;

            try {
                subscriptionPage = subscriptionRepositoryCustom.searchForSubscription(name, user, sort, sortColumn,
                        pageNumber, pageSize, true);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.ResultsFound);
            }

            if (subscriptionPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }

            List<SubscriptionDTO> subscriptionDTOs = subscriptionPage.getContent().stream()
                    .map(subscription -> SubscriptionDTO.builder()
                            .status(subscription.getStatus())
                            .id(subscription.getId())
                            .user(subscription.getUser().getUsername())
                            .name(subscription.getName().toUpperCase())
                            .months(subscription.getMonths())
                            .discountPercent(subscription.getDiscountPercent())
                            .registrationDate(subscription.getRegistrationDate())
                            .updateDate(subscription.getUpdateDate())
                            .build())
                    .toList();

            return new PageImpl<>(subscriptionDTOs, subscriptionPage.getPageable(), subscriptionPage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<PlanDTO>> listPlans() throws InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            try {
                List<Module> modules = moduleRepository.findAllByStatusTrue();
                List<Subscription> subscriptions = subscriptionRepository.findAllByStatusTrue();

                List<PlanDTO> plans = subscriptions.stream().map(subscription -> {

                    List<ModulePlanDTO> moduleList = modules.stream().map(module -> {

                        double discount = ((module.getMonthlyPrice() * subscription.getMonths())
                                * subscription.getDiscountPercent()) / 100;
                        BigDecimal discountedPrice = new BigDecimal((module.getMonthlyPrice() * subscription.getMonths()) -
                                discount).setScale(2, RoundingMode.HALF_EVEN);

                        return ModulePlanDTO.builder()
                                .moduleName(module.getName())
                                .modulePrice(discountedPrice)
                                .id(module.getId())
                                .status(module.getStatus())
                                .user(module.getUser().getUsername())
                                .build();

                    }).toList();

                    return PlanDTO.builder()
                            .name(subscription.getName())
                            .months(subscription.getMonths())
                            .discountPercentage(subscription.getDiscountPercent())
                            .id(subscription.getId())
                            .user(subscription.getUser().getUsername())
                            .status(subscription.getStatus())
                            .moduleList(moduleList)
                            .build();
                }).toList();

                return plans;
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }
}
