package com.proyect.masterdata.services.impl;

import java.util.List;
import java.util.concurrent.CompletableFuture;

import org.springframework.stereotype.Service;

import com.proyect.masterdata.domain.Category;
import com.proyect.masterdata.domain.Client;
import com.proyect.masterdata.domain.EntryChannel;
import com.proyect.masterdata.domain.Onboard;
import com.proyect.masterdata.domain.OnboardChannel;
import com.proyect.masterdata.domain.OnboardModule;
import com.proyect.masterdata.domain.Store;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.OnboardingDTO;
import com.proyect.masterdata.dto.request.RequestOnboard;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.CategoryRepository;
import com.proyect.masterdata.repository.ClientRepository;
import com.proyect.masterdata.repository.EntryChannelRepository;
import com.proyect.masterdata.repository.OnboardChannelRepository;
import com.proyect.masterdata.repository.OnboardModuleRepository;
import com.proyect.masterdata.repository.OnboardRepository;
import com.proyect.masterdata.repository.StoreRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IOnboard;
import com.proyect.masterdata.utils.Constants;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class OnboardImpl implements IOnboard {

    private final OnboardRepository onboardRepository;
    private final ClientRepository clientRepository;
    private final EntryChannelRepository entryChannelRepository;
    private final CategoryRepository categoryRepository;
    private final UserRepository userRepository;
    private final StoreRepository storeRepository;
    private final OnboardChannelRepository onboardChannelRepository;
    private final OnboardModuleRepository onboardModuleRepository;

    @Override
    public CompletableFuture<Onboard> save(RequestOnboard requestOnboard) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            EntryChannel entryChannel;
            Client client;
            Category category;

            try {
                entryChannel = entryChannelRepository
                        .findByNameAndStatusTrue(requestOnboard.getEntryChannel().toUpperCase());
                client = clientRepository.findByRucAndStatusTrue(requestOnboard.getBusinessRuc());
                category = categoryRepository.findByNameAndStatusTrue(requestOnboard.getCategory().toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (entryChannel == null) {
                throw new BadRequestExceptions(Constants.ErrorEntryChannel);
            }

            if (client == null) {
                throw new BadRequestExceptions(Constants.ErrorClient);
            }

            if (category == null) {
                throw new BadRequestExceptions(Constants.ErrorCategory);
            }

            try {

                String[] users = requestOnboard.getUsers().split("-");
                Integer usersMinimum = Integer.parseInt(users[0]);
                Integer usersMaximum = Integer.parseInt(users[1]);

                return onboardRepository.save(Onboard.builder()
                        .category(category)
                        .categoryId(category.getId())
                        .ecommerce(requestOnboard.getEcommerce())
                        .entryChannel(entryChannel)
                        .entryChannelId(entryChannel.getId())
                        .usersMinimum(usersMinimum)
                        .usersMaximum(usersMaximum)
                        .client(client)
                        .clientId(client.getId())
                        .demo(requestOnboard.getDemo())
                        .billing(requestOnboard.getBilling())
                        .comment(requestOnboard.getComments())
                        .build());

            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<List<OnboardingDTO>> listOnboard() throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            try {
                List<Onboard> onboards = onboardRepository.findAll();
                return onboards.stream().map(onboard -> {

                    OnboardingDTO onboardingDTO = OnboardingDTO.builder().build();

                    User user = userRepository.findByClientId(onboard.getClientId());

                    onboardingDTO.setUsername(user.getUsername());
                    onboardingDTO.setName(onboard.getClient().getName());
                    onboardingDTO.setSurname(onboard.getClient().getSurname());
                    onboardingDTO.setEmail(onboard.getClient().getEmail());
                    onboardingDTO.setAddress(onboard.getClient().getAddress());
                    onboardingDTO.setMobile(onboard.getClient().getMobile());
                    onboardingDTO.setDni(onboard.getClient().getDni());
                    onboardingDTO.setCategory(onboard.getCategory().getName());
                    onboardingDTO.setUsersMinimum(onboard.getUsersMinimum());
                    onboardingDTO.setUsersMaximum(onboard.getUsersMaximum());
                    onboardingDTO.setGender(user.getGender());
                    onboardingDTO.setDistrict(onboard.getClient().getDistrict().getName());

                    if (onboard.getEcommerce()) {
                        Store store = storeRepository.findByClientId(onboard.getClientId());
                        onboardingDTO.setStore(store.getName());
                        onboardingDTO.setStoreUrl(store.getUrl());
                        onboardingDTO.setStoreType(store.getStoreType().getName());
                    } else {
                        onboardingDTO.setStore("NO APLICA");
                        onboardingDTO.setStoreUrl("NO APLICA");
                        onboardingDTO.setStoreType("NO APLICA");
                    }

                    onboardingDTO.setComment(onboard.getComment());
                    onboardingDTO.setBilling(onboard.getBilling());
                    onboardingDTO.setBusinessRuc(onboard.getClient().getRuc());
                    onboardingDTO.setBusinessName(onboard.getClient().getBusiness());

                    List<OnboardChannel> onboardChannels = onboardChannelRepository.findByOnboardId(onboard.getId());
                    List<String> closingChannels = onboardChannels.stream()
                            .map(onboardChannel -> onboardChannel.getClosingChannel().getName()).toList();
                    onboardingDTO.setClosingChannels(closingChannels);

                    List<OnboardModule> onboardModules = onboardModuleRepository.findByOnboardId(onboard.getId());
                    List<String> modules = onboardModules.stream().map(onboardModule -> onboardModule.getModule().getName())
                            .toList();
                    onboardingDTO.setModules(modules);

                    onboardingDTO.setEntryChannel(onboard.getEntryChannel().getName());

                    onboardingDTO.setDemo(onboard.getDemo());

                    return onboardingDTO;
                }).toList();
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
        });
    }
}
