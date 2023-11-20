package com.proyect.masterdata.services.impl;

import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;

import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.security.SecurityUser;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class UserDetailsCustomImpl implements UserDetailsService {

    private final UserRepository userRepository;

    @Override
    public UserDetails loadUserByUsername(String username) throws UsernameNotFoundException {

        User user = userRepository.findByUsername(username);

        SecurityUser securityUser = new SecurityUser(user);

        if (user == null) {
            throw new UsernameNotFoundException("Username not found");
        }

        return securityUser;
    }
}
