package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.ChannelDTO;
import com.proyect.masterdata.dto.ChannelListDTO;
import com.proyect.masterdata.dto.request.RequestChannelSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.mapper.ChannelMapper;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IChannel;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.Date;
import java.util.List;

@Service
@RequiredArgsConstructor
@Log4j2
public class ChannelImpl implements IChannel {

    private final UserRepository userRepository;
    private final ChannelRepository channelRepository;
    private final ClientRepository clientRepository;
    private final MembershipRepository membershipRepository;
    private final ConnectionRepository connectionRepository;
    private final ChannelRepositoryCustom channelRepositoryCustom;
    private final StoreRepository clientChannelRepository;
    private final ModuleRepository moduleRepository;
    private final PaymentMethodRepository paymentMethodRepository;
    private final PaymentRepository paymentRepository;
    private final PaymentStateRepository paymentStateRepository;

    @Override
    public ResponseSuccess save(RequestChannelSave requestChannelSave, String user)
            throws InternalErrorExceptions, BadRequestExceptions {
        boolean existsUser;
        boolean channel;
        User userData;
        Client client;
        PaymentMethod paymentMethod;
        Connection connection;
        try {
            existsUser = userRepository.existsByUsername(user.toUpperCase());
            userData = userRepository.findByUsername(requestChannelSave.getUser().toUpperCase());
            channel = channelRepository.existsByName(requestChannelSave.getName().toUpperCase());
            client = clientRepository.findByRucAndStatusTrue(requestChannelSave.getClient().toUpperCase());
            paymentMethod = paymentMethodRepository
                    .findByNameAndStatusTrue(requestChannelSave.getPaymentMethod().toUpperCase());
            connection = connectionRepository.findByUrl(requestChannelSave.getConnection());
        } catch (RuntimeException e) {
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (!existsUser) {
            throw new BadRequestExceptions("Usuario autorizado no existe");
        }

        if (userData == null) {
            throw new BadRequestExceptions("Usuario no existe");
        }

        if (channel) {
            throw new BadRequestExceptions("Canal ya existe");
        }

        if (client == null) {
            throw new BadRequestExceptions("Cliente no existe");
        }

        try {
            channelRepository.save(Channel.builder()
                    .name(requestChannelSave.getName().toUpperCase())
                    .months(requestChannelSave.getMonths())
                    .client(client)
                    .idClient(client.getId())
                    .membership(null)
                    .idMembership(null)
                    .paymentMethod(paymentMethod)
                    .idPaymentMethod(paymentMethod.getId())
                    .connection(connection)
                    .idConnection(connection.getIdConnection())
                    .datauser(userData)
                    .user(userData.getUsername().toUpperCase())
                    .status(true)
                    .dateRegistration(new Date(System.currentTimeMillis()))
                    .build());
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
    public ResponseSuccess saveAll(List<RequestChannelSave> requestChannelSaveList, String user)
            throws InternalErrorExceptions, BadRequestExceptions {
        boolean existsUser;
        List<Channel> channelList;
        List<User> userList;
        List<Client> clientList;
        List<PaymentMethod> paymentMethodList;
        List<Connection> connectionList;
        try {
            existsUser = userRepository.existsByUsername(user.toUpperCase());
            channelList = channelRepository.findByNameIn(
                    requestChannelSaveList.stream().map(channel -> channel.getName().toUpperCase()).toList());
            userList = userRepository.findByUsernameIn(
                    requestChannelSaveList.stream().map(userData -> userData.getUser().toUpperCase()).toList());
            clientList = clientRepository
                    .findByRucIn(requestChannelSaveList.stream().map(client -> client.getClient()).toList());
            paymentMethodList = paymentMethodRepository.findByNameIn(requestChannelSaveList.stream()
                    .map(paymentMethod -> paymentMethod.getPaymentMethod().toUpperCase()).toList());
            connectionList = connectionRepository.findByUrlIn(
                    requestChannelSaveList.stream().map(connection -> connection.getConnection()).toList());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if (!existsUser) {
            throw new BadRequestExceptions("Usuario autorizado no existe");
        }
        if (!channelList.isEmpty()) {
            throw new BadRequestExceptions("Canal existente");
        }
        if (userList.size() != requestChannelSaveList.size()) {
            throw new BadRequestExceptions("Usuario no existe");
        }
        if (clientList.size() != requestChannelSaveList.size()) {
            throw new BadRequestExceptions("Cliente no existe");
        }
        if (paymentMethodList.size() != requestChannelSaveList.size()) {
            throw new BadRequestExceptions("Tipo de pago no existe");
        }
        if (connectionList.size() != requestChannelSaveList.size()) {
            throw new BadRequestExceptions("Conexion no existe");
        }

        try {
            channelRepository.saveAll(requestChannelSaveList.stream().map(channel -> {
                return Channel.builder()
                        .name(channel.getName().toUpperCase())
                        .months(channel.getMonths())
                        .client(clientRepository.findByRucAndStatusTrue(channel.getClient()))
                        .idClient(clientRepository.findByRucAndStatusTrue(channel.getClient()).getId())
                        .membership(null)
                        .idMembership(null)
                        .paymentMethod(paymentMethodRepository
                                .findByNameAndStatusTrue(channel.getPaymentMethod().toUpperCase()))
                        .idPaymentMethod(paymentMethodRepository
                                .findByNameAndStatusTrue(channel.getPaymentMethod().toUpperCase()).getId())
                        .connection(connectionRepository.findByUrl(channel.getConnection()))
                        .idConnection(connectionRepository.findByUrl(channel.getConnection()).getIdConnection())
                        .user(channel.getUser().toUpperCase())
                        .dateRegistration(new Date(System.currentTimeMillis()))
                        .status(true)
                        .build();
            }).toList());
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
    public ChannelDTO update(String name, Integer months, String user)
            throws InternalErrorExceptions, BadRequestExceptions {
        boolean existsUser;
        Channel channel;
        User userData;
        Client client;
        Membership membership;
        PaymentMethod paymentMethod;
        Connection connection;
        try {
            existsUser = userRepository.existsByUsername(user.toUpperCase());
            channel = channelRepository.findByName(name.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if (!existsUser) {
            throw new BadRequestExceptions("Usuario no existe");
        }
        if (channel == null) {
            throw new BadRequestExceptions("Canal no existe");
        }
        try {
            client = clientRepository.findByRucAndStatusTrue(channel.getClient().getRuc());
            membership = membershipRepository.findById(channel.getIdMembership()).orElse(null);
            paymentMethod = paymentMethodRepository.findByNameAndStatusTrue(channel.getPaymentMethod().getName());
            connection = connectionRepository.findByUrl(channel.getConnection().getUrl());
            userData = userRepository.findByUsername(channel.getUser().toUpperCase());
            channel.setMonths(months);
            channel.setDateRegistration(new Date(System.currentTimeMillis()));
            channel.setStatus(true);
            channelRepository.save(channel);
            return ChannelDTO.builder()
                    .name(channel.getName())
                    .months(channel.getMonths())
                    .client(client.getRuc())
                    .membership(membership.getId())
                    .paymentMethod(paymentMethod.getName())
                    .connection(connection.getUrl())
                    .user(userData.getUsername())
                    .build();
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public ResponseDelete delete(String name, String user) throws InternalErrorExceptions, BadRequestExceptions {
        boolean existsUser;
        Channel channel;
        try {
            existsUser = userRepository.existsByUsername(user.toUpperCase());
            channel = channelRepository.findByName(name.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if (!existsUser) {
            throw new BadRequestExceptions("Usuario no existe");
        }
        if (channel == null) {
            throw new BadRequestExceptions("Canal no existe");
        }
        try {
            channel.setStatus(false);
            channel.setDateRegistration(new Date(System.currentTimeMillis()));
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public Page<ChannelListDTO> list(String name, String user, String sort, String sortColumn, Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {
        Page<Channel> channelPage;
        try {
            channelPage = channelRepositoryCustom.searchForChannel(name, user, sort, sortColumn, pageNumber, pageSize,
                    true);
        } catch (RuntimeException e) {
            log.error(e);
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
        if (channelPage.isEmpty()) {
            return new PageImpl<>(Collections.emptyList());
        }
        List<ChannelListDTO> channelDTOList = channelPage.getContent().stream().map(channel -> {
            Store clientChannel = clientChannelRepository.findById(channel.getId()).orElse(null);
            // replace with the name of success state in payment states
            List<MembershipPayment> paymentList = paymentRepository.findByIdChannelAndIdPaymentState(channel.getId(),
                    paymentStateRepository.findByNameAndStatusTrue("ACEPTADO").getId());
            return ChannelListDTO.builder()
                    .name(channel.getName().toUpperCase())
                    .subscribedMonths(channel.getMonths())
                    .client(channel.getClient().getRuc())
                    .membership(channel.getIdMembership())
                    .connection(channel.getConnection().getUrl())
                    .paymentMethod(channel.getPaymentMethod().getName())
                    .user(channel.getUser().toUpperCase())
                    .ecommerce(clientChannel.getName())
                    .payedMonths(paymentList.size())
                    .build();
        }).toList();
        return new PageImpl<>(channelDTOList,
                channelPage.getPageable(), channelPage.getTotalElements());
    }

    @Override
    public Page<ChannelListDTO> listStatusFalse(String name, String user, String sort, String sortColumn,
            Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        Page<Channel> channelPage;
        try {
            channelPage = channelRepositoryCustom.searchForChannel(name, user, sort, sortColumn, pageNumber, pageSize,
                    false);
        } catch (RuntimeException e) {
            log.error(e);
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
        if (channelPage.isEmpty()) {
            return new PageImpl<>(Collections.emptyList());
        }
        List<ChannelListDTO> channelDTOList = channelPage.getContent().stream().map(channel -> {
            Store clientChannel = clientChannelRepository.findById(channel.getId()).orElse(null);
            // replace with the name of success state in payment states
            List<MembershipPayment> paymentList = paymentRepository.findByIdChannelAndIdPaymentState(channel.getId(),
                    paymentStateRepository.findByNameAndStatusTrue("ACEPTADO").getId());
            return ChannelListDTO.builder()
                    .name(channel.getName().toUpperCase())
                    .subscribedMonths(channel.getMonths())
                    .client(channel.getClient().getRuc())
                    .membership(channel.getIdMembership())
                    .connection(channel.getConnection().getUrl())
                    .paymentMethod(channel.getPaymentMethod().getName())
                    .ecommerce(clientChannel.getName())
                    .user(channel.getUser().toUpperCase())
                    .build();
        }).toList();
        return new PageImpl<>(channelDTOList,
                channelPage.getPageable(), channelPage.getTotalElements());
    }
}
