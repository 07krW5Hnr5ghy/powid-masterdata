package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Module;
import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.MembershipDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IMembership;
import com.proyect.masterdata.services.IUserRole;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class MembershipImpl implements IMembership {

    private final MembershipRepository membershipRepository;
    private final UserRepository userRepository;
    private final ModuleRepository moduleRepository;
    private final ClientRepository clientRepository;
    private final MembershipRepositoryCustom membershipRepositoryCustom;
    private final SubscriptionRepository subscriptionRepository;
    private final MembershipModuleRepository membershipModuleRepository;
    private final MembershipStateRepository membershipStateRepository;
    private final UserRoleRepository userRoleRepository;
    private final RoleRepository roleRepository;
    private final IUserRole iUserRole;
    private final IAudit iAudit;
    @Override
    public CompletableFuture<Membership> save(User user, MembershipPayment membershipPayment, String subscriptionName, List<String> modules, Boolean demo, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Subscription subscription;
            MembershipState activeState;
            MembershipState payedState;
            Role role;
            UserRole userRole = null;

            try {
                subscription = subscriptionRepository.findByNameAndStatusTrue(subscriptionName.toUpperCase());
                activeState = membershipStateRepository.findByNameAndStatusTrue("ACTIVA");
                payedState = membershipStateRepository.findByNameAndStatusTrue("PAGADA");
                role = roleRepository.findByNameAndStatusTrue("BUSINESS");
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (subscription == null) {
                throw new BadRequestExceptions(Constants.ErrorSubscription);
            }

            if(role != null){
                userRole = userRoleRepository.findByUserIdAndRoleIdAndStatusTrue(user.getId(),role.getId());
            }

            try {
                Calendar calendar = Calendar.getInstance();
                calendar.add(Calendar.MONTH, subscription.getMonths());
                Date expirationDate = calendar.getTime();
                Membership newMembership =  membershipRepository.save(Membership.builder()
                        .clientId(user.getClientId())
                        .client(user.getClient())
                        .demo(demo)
                        .expirationDate(expirationDate)
                        .membershipPayment(membershipPayment)
                        .membershipPaymentId(membershipPayment.getId())
                        .updateDate(new Date(System.currentTimeMillis()))
                        .subscription(subscription)
                        .subscriptionId(subscription.getId())
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .build());
                newMembership.setExpirationDate(expirationDate);
                Membership activeMembership = membershipRepository.findByClientIdAndMembershipStateId(user.getClientId(), activeState.getId());
                if(activeMembership == null){
                    newMembership.setMembershipState(activeState);
                    newMembership.setMembershipStateId(activeState.getId());
                }
                Membership payedMembership = membershipRepository.findByClientIdAndMembershipStateId(user.getClientId(), payedState.getId());
                if(payedMembership == null){
                    newMembership.setMembershipState(payedState);
                    newMembership.setMembershipStateId(payedState.getId());
                }else{
                    throw new BadRequestExceptions(Constants.ErrorMembershipActivePayed);
                }

                membershipRepository.save(newMembership);
                for(String moduleName : modules){
                    Module module = moduleRepository.findByNameAndStatusTrue(moduleName);
                    membershipModuleRepository.save(MembershipModule.builder()
                            .membership(newMembership)
                            .membershipId(newMembership.getId())
                            .module(module)
                            .moduleId(module.getId())
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .updateDate(new Date(System.currentTimeMillis()))
                            .status(true)
                            .build());
                }
                if(userRole == null){
                    iUserRole.save(user.getUsername(),"BUSINESS",user.getUsername());
                }
                iAudit.save("ADD_MEMBERSHIP","ADD MEMBERSHIP WITH SUBSCRIPTION "+newMembership.getSubscription()+".",user.getUsername());
                return newMembership;
            } catch (RuntimeException e) {
                e.printStackTrace();
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseDelete> delete(String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Membership membership;
            MembershipState membershipState;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                membershipState = membershipStateRepository.findByNameAndStatusTrue("ACTIVA");
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else {
                membership = membershipRepository.findByClientIdAndMembershipStateId(user.getClientId(), membershipState.getId());
            }

            if (membership == null) {
                throw new BadRequestExceptions(Constants.ErrorMembership);
            }

            try {
                MembershipState expiredState = membershipStateRepository.findByNameAndStatusTrue("EXPIRADA");
                membership.setMembershipState(expiredState);
                membership.setMembershipStateId(expiredState.getId());
                membership.setUpdateDate(new Date(System.currentTimeMillis()));
                membershipRepository.save(membership);
                iAudit.save("DELETE_MEMBERSHIP","DELETE MEMBERSHIP WITH SUBSCRIPTION "+membership.getSubscription()+".",user.getUsername());
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
    public CompletableFuture<Page<MembershipDTO>> list(
            String user,
            String membershipState,
            String subscription,
            Date registrationStartDate,
            Date registrationEndDate,
            Date updateStartDate,
            Date updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<Membership> membershipPage;
            Long clientId;
            Long membershipStateId;
            Long subscriptionId;

            if(membershipState != null){
                membershipStateId = membershipStateRepository.findByNameAndStatusTrue(membershipState.toUpperCase()).getId();
            }else{
                membershipStateId = null;
            }

            if(subscription != null){
                subscriptionId = subscriptionRepository.findByNameAndStatusTrue(subscription.toUpperCase()).getId();
            }else {
                subscriptionId = null;
            }

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                membershipPage = membershipRepositoryCustom.searchForMembership(
                        clientId,
                        membershipStateId,
                        subscriptionId,
                        registrationStartDate,
                        registrationEndDate,
                        updateStartDate,
                        updateStartDate,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        true);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if (membershipPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }
            List<MembershipDTO> membershipDTOS = membershipPage.getContent().stream().map(membership -> MembershipDTO.builder()
                    .membershipState(membership.getMembershipState().getName())
                    .subscription(membership.getSubscription().getName())
                    .registrationDate(membership.getRegistrationDate())
                    .expirationDate(membership.getExpirationDate())
                    .build()).toList();
            return new PageImpl<>(membershipDTOS, membershipPage.getPageable(), membershipPage.getTotalElements());
        });
    }
    @Override
    public CompletableFuture<Page<MembershipDTO>> listFalse(
            String user,
            String membershipState,
            String subscription,
            Date registrationStartDate,
            Date registrationEndDate,
            Date updateStartDate,
            Date updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<Membership> membershipPage;
            Long clientId;
            Long membershipStateId;
            Long subscriptionId;

            if(membershipState != null){
                membershipStateId = membershipStateRepository.findByNameAndStatusTrue(membershipState.toUpperCase()).getId();
            }else{
                membershipStateId = null;
            }

            if(subscription != null){
                subscriptionId = subscriptionRepository.findByNameAndStatusTrue(subscription.toUpperCase()).getId();
            }else {
                subscriptionId = null;
            }

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                membershipPage = membershipRepositoryCustom.searchForMembership(
                        clientId,
                        membershipStateId,
                        subscriptionId,
                        registrationStartDate,
                        registrationEndDate,
                        updateStartDate,
                        updateStartDate,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        false);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if (membershipPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }
            List<MembershipDTO> membershipDTOS = membershipPage.getContent().stream().map(membership -> MembershipDTO.builder()
                    .membershipState(membership.getMembershipState().getName())
                    .subscription(membership.getSubscription().getName())
                    .registrationDate(membership.getRegistrationDate())
                    .expirationDate(membership.getExpirationDate())
                    .build()).toList();
            return new PageImpl<>(membershipDTOS, membershipPage.getPageable(), membershipPage.getTotalElements());
        });
    }
}
