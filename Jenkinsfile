pipeline {
  agent {
    label 'nix'
  }

  stages {
    stage('Build and Test') {
      steps {
        sh 'nix-build --arg inCI true build.nix'
      }

      options {
        timeout(time: 60, unit: 'MINUTES')
      }
    }
  }
}