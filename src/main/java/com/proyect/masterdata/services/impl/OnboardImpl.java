package com.proyect.masterdata.services.impl;

import org.springframework.stereotype.Service;

import com.proyect.masterdata.domain.Category;
import com.proyect.masterdata.domain.Client;
import com.proyect.masterdata.domain.EntryChannel;
import com.proyect.masterdata.domain.Onboard;
import com.proyect.masterdata.dto.request.RequestOnboard;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.CategoryRepository;
import com.proyect.masterdata.repository.ClientRepository;
import com.proyect.masterdata.repository.EntryChannelRepository;
import com.proyect.masterdata.repository.OnboardRepository;
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

    @Override
    public Onboard save(RequestOnboard requestOnboard) throws InternalErrorExceptions, BadRequestExceptions {

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

            Onboard onboard = onboardRepository.save(Onboard.builder()
                    .category(category)
                    .idCategory(category.getId())
                    .ecommerce(requestOnboard.getEcommerce())
                    .entryChannel(entryChannel)
                    .idEntryChannel(entryChannel.getId())
                    .users(requestOnboard.getUsers())
                    .client(client)
                    .idClient(client.getId())
                    .demo(requestOnboard.getDemo())
                    .billing(requestOnboard.getBilling())
                    .comment(requestOnboard.getComments())
                    .build());

            return onboard;

        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

}
