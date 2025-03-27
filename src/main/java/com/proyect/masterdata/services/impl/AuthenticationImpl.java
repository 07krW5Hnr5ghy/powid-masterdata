package com.proyect.masterdata.services.impl;

import java.time.OffsetDateTime;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.domain.Module;
import com.proyect.masterdata.dto.LoginDTO;
import com.proyect.masterdata.dto.projections.ProvinceDTOP;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.*;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.crypto.password.PasswordEncoder;
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
    private final OnboardRepository onboardRepository;
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
    private final IAudit iAudit;
    private final UserRoleRepository userRoleRepository;
    private final ProvinceRepository provinceRepository;
    private final PasswordEncoder passwordEncoder;
    private final RoleAccessRepository roleAccessRepository;
    private final RoleRepository roleRepository;
    private final AccessRepository accessRepository;
    private final UserRoleRepository userRoleUserRepository;

    public CompletableFuture<LoginDTO> loginUser(String username, String password) {
        return CompletableFuture.supplyAsync(() -> {
            try {

                User user;
                Membership activeMembership;
                Membership payedMembership;
                MembershipState activeState;
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                List<User> users = userRepository.findAll();
                activeState = membershipStateRepository.findByNameAndStatusTrue("ACTIVA");
                Date currentDate = new Date(System.currentTimeMillis());
                MembershipState payedState = membershipStateRepository.findByNameAndStatusTrue("PAGADA");
                MembershipState expiredState = membershipStateRepository.findByNameAndStatusTrue("EXPIRADA");
                if (user == null) {
                    throw new BadRequestExceptions(Constants.ErrorAuthentication);
                }else {
                    //activeMembership = membershipRepository.findByClientIdAndMembershipStateId(user.getId(), activeState.getId());
                }

//                if(activeMembership != null){
//                    if(activeMembership.getExpirationDate().compareTo(currentDate) < 0){
//                        iMembership.delete(user.getUsername());
//                        payedMembership = membershipRepository.findByClientIdAndMembershipStateId(user.getClientId(),payedState.getId());
//                        if(payedMembership != null){
//                            payedMembership.setMembershipState(activeState);
//                            payedMembership.setMembershipStateId(activeState.getId());
//                            membershipRepository.save(payedMembership);
//                        }
//                    }
//                }

                Authentication auth = authenticationManager.authenticate(
                        new UsernamePasswordAuthenticationToken(username.toUpperCase(), password));

                String token = iToken.generateJwt(auth);

                iAudit.save("LOG_IN","USUARIO " + user.getUsername() + " INICIO SESION.",user.getUsername(),user.getUsername());
                User userData = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                List<UserRole> userRoles = userRoleRepository.findByUserIdAndStatusTrue(userData.getId());

                return LoginDTO.builder()
                        .username(userData.getUsername())
                        .jwt(token)
                        .roles(userRoles.stream().map(role -> role.getRole().getName()).toList())
                        .code(200)
                        .message("Inicio de sesion exitoso")
                        .build();
            } catch (AuthenticationException e) {
                log.error(e.getStackTrace());
                throw new BadRequestExceptions(Constants.ErrorAuthentication);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> registerNewClient(RequestOnboarding requestOnboarding)
            throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(() -> {
            String roleName = "NEGOCIO";
            String accesName = "USER_GET";
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
            Province province;
            ProvinceDTOP provinceDTOP;
            Role role;
            Access access;
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
                role = roleRepository.findByName(roleName);
                access = accessRepository.findByName(accesName);
                //provinceDTOP = provinceRepository.findByName(requestOnboarding.getProvince());
                province = provinceRepository.findByNameAndStatusTrue(requestOnboarding.getProvince().toUpperCase());
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

            if(province == null){
                throw new BadRequestExceptions(Constants.ErrorProvince);
            }else{
                district = districtRepository.findByNameAndProvinceIdAndStatusTrue(requestOnboarding.getDistrict().toUpperCase(),province.getId());
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

//                RequestClientSave requestClientSave = RequestClientSave.builder()
//                        .name(requestOnboarding.getName().toUpperCase())
//                        .surname(requestOnboarding.getSurname().toUpperCase())
//                        .business(requestOnboarding.getBusinessName().toUpperCase())
//                        .address(requestOnboarding.getAddress().toUpperCase())
//                        .dni(requestOnboarding.getDni())
//                        .email(requestOnboarding.getEmail())
//                        .mobile(requestOnboarding.getMobile())
//                        .district(requestOnboarding.getDistrict().toUpperCase())
//                        .ruc(requestOnboarding.getBusinessRuc())
//                        .build();
//
//                iClient.save(requestClientSave);

                //implementacion que sirve, pero que no almacena al cliente con el admiin
//                Client saveClient = clientRepository.save(Client.builder()
//                                .name(requestOnboarding.getName().toUpperCase())
//                                .surname(requestOnboarding.getSurname().toUpperCase())
//                                .ruc(requestOnboarding.getBusinessRuc())
//                                .dni(requestOnboarding.getDni())
//                                .business(requestOnboarding.getBusinessName().toUpperCase())
//                                .mobile(requestOnboarding.getMobile())
//                                .address(requestOnboarding.getAddress().toUpperCase())
//                                .email(requestOnboarding.getEmail())
//                                .status(true)
//                                .district(district)
//                                .districtId(district.getId())
//                                .registrationDate(OffsetDateTime.now())
//                                .updateDate(OffsetDateTime.now())
//                        .build()
//                );

                //nueva implementacion
                Client newClient = clientRepository.save(Client.builder()
                        .name(requestOnboarding.getName().toUpperCase())
                        .surname(requestOnboarding.getSurname().toUpperCase())
                        .ruc(requestOnboarding.getBusinessRuc())
                        .dni(requestOnboarding.getDni())
                        .business(requestOnboarding.getBusinessName().toUpperCase())
                        .mobile(requestOnboarding.getMobile())
                        .address(requestOnboarding.getAddress().toUpperCase())
                        .email(requestOnboarding.getEmail())
                        .status(true)
                        .district(district)
                        .districtId(district.getId())
                        .registrationDate(OffsetDateTime.now())
                        .updateDate(OffsetDateTime.now())
                        .build());

                System.out.println("Client saved: " + newClient);

                // antigua implementacion
//                RequestUser requestUser = RequestUser.builder()
//                        .user(requestOnboarding.getUsername().toUpperCase())
//                        .name(requestOnboarding.getName().toUpperCase())
//                        .surname(requestOnboarding.getSurname().toUpperCase())
//                        .address(requestOnboarding.getAddress().toUpperCase())
//                        .dni(requestOnboarding.getDni())
//                        .gender(requestOnboarding.getGender().toUpperCase())
//                        .mobile(requestOnboarding.getMobile())
//                        .password(requestOnboarding.getPassword())
//                        .email(requestOnboarding.getEmail())
//                        .province(requestOnboarding.getProvince())
//                        .district(requestOnboarding.getDistrict().toUpperCase())
//                        .roleName("NEGOCIO")
//                        .tokenUser("JROMERO")
//                        .build();

                //crear un metodo para asignar el cliente al usuario

                //forma de como agregar un cliente a un usuario
                        User newUser = userRepository.save(User.builder()
                                .username(requestOnboarding.getUsername())
                                .name(requestOnboarding.getName())
                                .surname(requestOnboarding.getSurname())
                                .dni(requestOnboarding.getDni())
                                .email(requestOnboarding.getEmail())
                                .address(requestOnboarding.getAddress())
                                .gender(requestOnboarding.getGender())
                                .district(district)
                                .districtId(district.getId())
                                .client(newClient)
                                .clientId(newClient.getId())
                                .mobile(requestOnboarding.getMobile())
                                .password(passwordEncoder.encode(requestOnboarding.getPassword()))
                                .status(true)
                                .registrationDate(OffsetDateTime.now())
                                .updateDate(OffsetDateTime.now())
                                .build());

                roleAccessRepository.save(RoleAccess.builder()
                                .userId(newUser.getId())
                                .user(newUser)
                                .role(role)
                                .roleId(role.getId())
                                .status(true)
                                .access(access)
                                .accessId(access.getId())
                                .registrationDate(OffsetDateTime.now())
                                .updateDate(OffsetDateTime.now())
                        .build());

                userRoleRepository.save(UserRole.builder()
                                .userId(newUser.getId())
                                .user(newUser)
                                .role(role)
                                .roleId(role.getId())
                                .status(true)
                                .registrationDate(OffsetDateTime.now())
                        .build());
                //iUser.save(requestUser);
                System.out.println("Save user -> " + newUser);

                Onboard onboard = iOnboard.save(RequestOnboard.builder()
                        .businessRuc(requestOnboarding.getBusinessRuc())
                        .billing(requestOnboarding.getBilling())
                        .ecommerce(requestOnboarding.getEcommerce())
                        .category(requestOnboarding.getCategory().toUpperCase())
                        .entryChannel(requestOnboarding.getEntryChannel())
                        .users(requestOnboarding.getUsers().toUpperCase())
                        .comments(requestOnboarding.getComment())
                        .demo(requestOnboarding.getDemo())
                        .build()).get();

                System.out.println(onboard);
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

                    iStore.save(requestStoreSave, requestOnboarding.getUsername().toUpperCase());

                    Store store = storeRepository.findByNameAndStatusTrue(requestOnboarding.getStore().toUpperCase());

                    iOnboardStore.save(store, onboard);

                }
                iAudit.save("REGISTER_CLIENT","NUEVO CLIENTE REGISTRADO CON EL RUC : " + newClient.getRuc() + " .",newClient.getRuc(), newUser.getUsername());

                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();

            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            } catch (ExecutionException | InterruptedException e) {
                throw new RuntimeException(e);
            }
        });
    }
}
