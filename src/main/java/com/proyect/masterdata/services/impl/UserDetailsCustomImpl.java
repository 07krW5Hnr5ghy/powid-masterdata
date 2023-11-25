package com.proyect.masterdata.services.impl;

import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;

import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.repository.AccessRepository;
import com.proyect.masterdata.repository.RoleAccessRepository;
import com.proyect.masterdata.repository.RoleRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.repository.UserRoleRepository;
import com.proyect.masterdata.security.SecurityUser;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class UserDetailsCustomImpl implements UserDetailsService {

    private final UserRepository userRepository;
    private final UserRoleRepository userRoleRepository;
    private final RoleRepository roleRepository;
    private final RoleAccessRepository roleAccessRepository;
    private final AccessRepository AccessRepository;

    @Override
    public UserDetails loadUserByUsername(String username) throws UsernameNotFoundException {

        User user = userRepository.findByUsername(username);

        SecurityUser securityUser = new SecurityUser(user, userRoleRepository, roleRepository, roleAccessRepository,
                AccessRepository);

        if (user == null) {
            throw new UsernameNotFoundException("Username not found");
        }

        return securityUser;
    }
}
