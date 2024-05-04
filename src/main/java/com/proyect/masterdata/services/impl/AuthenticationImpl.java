package com.proyect.masterdata.services.impl;

import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.domain.Module;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.*;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.stereotype.Service;

import com.proyect.masterdata.dto.request.RequestClientSave;
import com.proyect.masterdata.dto.request.RequestOnboard;
import com.proyect.masterdata.dto.request.RequestOnboarding;
import com.proyect.masterdata.dto.request.RequestStoreSave;
import com.proyect.masterdata.dto.request.RequestUser;
import com.proyect.masterdata.dto.response.ResponseLogin;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.utils.Constants;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class AuthenticationImpl implements IAuthentication {

    private final AuthenticationManager authenticationManager;
    private final IToken iToken;
    private final UserRepository userRepository;
    private final ClientRepository clientRepository;
    private final IUser iUser;
    private final DistrictRepository districtRepository;
    private final IClient iClient;
    private final IOnboard iOnboard;
    private final ClosingChannelRepository closingChannelRepository;
    private final IOnboardChannel iOnboardChannel;
    private final IStore iStore;
    private final StoreRepository storeRepository;
    private final IOnboardStore iOnboardStore;
    private final CategoryRepository categoryRepository;
    private final ModuleRepository moduleRepository;
    private final IOnboardModule iOnboardModule;
    private final IMembership iMembership;
    private final MembershipRepository membershipRepository;
    private final MembershipStateRepository membershipStateRepository;

    public CompletableFuture<ResponseLogin> loginUser(String username, String password) {
        return CompletableFuture.supplyAsync(() -> {
            try {

                User user;
                Membership activeMembership;
                Membership payedMembership;
                MembershipState activeState;

                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                activeState = membershipStateRepository.findByNameAndStatusTrue("ACTIVA");
                Date currentDate = new Date(System.currentTimeMillis());
                MembershipState payedState = membershipStateRepository.findByNameAndStatusTrue("PAGADA");
                MembershipState expiredState = membershipStateRepository.findByNameAndStatusTrue("EXPIRADA");

                if (user == null) {
                    throw new BadRequestExceptions(Constants.ErrorAuthentication);
                }else {
                    activeMembership = membershipRepository.findByClientIdAndMembershipStateId(user.getId(), activeState.getId());
                }

                if(activeMembership != null){
                    if(activeMembership.getExpirationDate().compareTo(currentDate) < 0){
                        iMembership.delete(user.getUsername());
                        payedMembership = membershipRepository.findByClientIdAndMembershipStateId(user.getClientId(),payedState.getId());
                        if(payedMembership != null){
                            payedMembership.setMembershipState(activeState);
                            payedMembership.setMembershipStateId(activeState.getId());
                            membershipRepository.save(payedMembership);
                        }
                    }
                }

                Authentication auth = authenticationManager.authenticate(
                        new UsernamePasswordAuthenticationToken(username.toUpperCase(), password));

                String token = iToken.generateJwt(auth);

                return new ResponseLogin(userRepository.findByUsernameAndStatusTrue(username.toUpperCase()), token);

            } catch (AuthenticationException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ErrorAuthentication);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> registerUser(RequestOnboarding requestOnboarding)
            throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(() -> {
            boolean existsUser = false;
            boolean existsUserDni = false;
            boolean existsUserEmail = false;
            boolean existsUserMobile = false;
            boolean existsClientRuc = false;
            boolean existsClientDni = false;
            boolean existsClientEmail = false;
            boolean existsClientMobile = false;
            boolean category = false;
            District district = null;
            List<ClosingChannel> closingChannels;
            List<Module> modules;

            try {
                existsUser = userRepository.existsByUsername(requestOnboarding.getUsername().toUpperCase());
                existsUserDni = userRepository.existsByDni(requestOnboarding.getDni());
                existsUserEmail = userRepository.existsByEmail(requestOnboarding.getEmail());
                existsUserMobile = userRepository.existsByMobile(requestOnboarding.getMobile());
                existsClientRuc = clientRepository.existsByRuc(requestOnboarding.getBusinessRuc());
                existsClientDni = clientRepository.existsByDni(requestOnboarding.getDni());
                existsClientEmail = clientRepository.existsByEmail(requestOnboarding.getEmail());
                existsClientMobile = clientRepository.existsByMobile(requestOnboarding.getMobile());
                district = districtRepository.findByNameAndStatusTrue(requestOnboarding.getDistrict().toUpperCase());
                closingChannels = closingChannelRepository.findByNameInAndStatusTrue(
                        requestOnboarding.getClosingChannels().stream().map(name -> name.toUpperCase()).toList());
                category = categoryRepository.existsByNameAndStatusTrue(requestOnboarding.getCategory().toUpperCase());
                modules = moduleRepository
                        .findByNameIn(requestOnboarding.getModules().stream().map(module -> module.toUpperCase()).toList());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (existsUser) {
                throw new BadRequestExceptions(Constants.ErrorUserExist);
            }

            if (existsUserDni) {
                throw new BadRequestExceptions(Constants.ErrorUserDniExist);
            }

            if (existsUserEmail) {
                throw new BadRequestExceptions(Constants.ErrorUserEmailExist);
            }

            if (existsUserMobile) {
                throw new BadRequestExceptions(Constants.ErrorUserMobileExist);
            }

            if (existsClientRuc) {
                throw new BadRequestExceptions(Constants.ErrorClientRucExist);
            }

            if (existsClientDni) {
                throw new BadRequestExceptions(Constants.ErrorClientDniExist);
            }

            if (existsClientEmail) {
                throw new BadRequestExceptions(Constants.ErrorClientEmailExist);
            }

            if (existsClientMobile) {
                throw new BadRequestExceptions(Constants.ErrorClientMobileExist);
            }

            if (district == null) {
                throw new BadRequestExceptions(Constants.ErrorDistrict);
            }

            if (closingChannels.size() != requestOnboarding.getClosingChannels().size()) {
                throw new BadRequestExceptions(Constants.ErrorClosingChannel);
            }

            if (modules.size() != requestOnboarding.getModules().size()) {
                throw new BadRequestExceptions(Constants.ErrorClosingChannel);
            }

            if (!category) {
                throw new BadRequestExceptions(Constants.ErrorCategory);
            }

            try {

                RequestClientSave requestClientSave = RequestClientSave.builder()
                        .name(requestOnboarding.getName().toUpperCase())
                        .surname(requestOnboarding.getSurname().toUpperCase())
                        .business(requestOnboarding.getBusinessName().toUpperCase())
                        .address(requestOnboarding.getAddress().toUpperCase())
                        .dni(requestOnboarding.getDni())
                        .email(requestOnboarding.getEmail())
                        .mobile(requestOnboarding.getMobile())
                        .district(requestOnboarding.getDistrict().toUpperCase())
                        .ruc(requestOnboarding.getBusinessRuc())
                        .build();

                iClient.save(requestClientSave);

                RequestUser requestUser = RequestUser.builder()
                        .user(requestOnboarding.getUsername().toUpperCase())
                        .name(requestOnboarding.getName().toUpperCase())
                        .surname(requestOnboarding.getSurname().toUpperCase())
                        .address(requestOnboarding.getAddress().toUpperCase())
                        .dni(requestOnboarding.getDni())
                        .gender(requestOnboarding.getGender().toUpperCase())
                        .mobile(requestOnboarding.getMobile())
                        .password(requestOnboarding.getPassword())
                        .email(requestOnboarding.getEmail())
                        .district(requestOnboarding.getDistrict().toUpperCase())
                        .roleName("BUSINESS")
                        .tokenUser("REGISTER")
                        .build();

                iUser.save(requestUser);

                Onboard onboard = iOnboard.save(RequestOnboard.builder()
                        .businessRuc(requestOnboarding.getBusinessRuc())
                        .billing(requestOnboarding.getBilling())
                        .ecommerce(requestOnboarding.getEcommerce())
                        .category(requestOnboarding.getCategory().toUpperCase())
                        .entryChannel(requestOnboarding.getEntryChannel())
                        .users(requestOnboarding.getUsers())
                        .comments(requestOnboarding.getComment())
                        .demo(requestOnboarding.getDemo())
                        .build());

                for (ClosingChannel closingChannel : closingChannels) {
                    iOnboardChannel.save(onboard, closingChannel);
                }

                for (Module module : modules) {
                    iOnboardModule.save(onboard, module);
                }

                if (requestOnboarding.getEcommerce()) {
                    RequestStoreSave requestStoreSave = RequestStoreSave.builder()
                            .name(requestOnboarding.getStore().toUpperCase())
                            .storeType(requestOnboarding.getStoreType().toUpperCase())
                            .url(requestOnboarding.getStoreUrl())
                            .build();

                    iStore.save(requestStoreSave, "REGISTER");

                    Store store = storeRepository.findByNameAndStatusTrue(requestOnboarding.getStore().toUpperCase());

                    iOnboardStore.save(store, onboard);

                }

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
}
